
module Data.Repa.Convert.Format.Base
        ( Format   (..)
        , Packable (..)
        , packToList
        , unpackFromList

        -- * Strict products
        , (:*:)(..)

        -- * Lists
        , FixList(..)
        , VarList(..)

        -- * ASCII Strings
        , FixString     (..)
        , VarString     (..)
        , ASCII         (..))

where
import Data.Repa.Product
import Data.Word
import Data.Char
import System.IO.Unsafe
import qualified Foreign.Storable               as S
import qualified Foreign.Marshal.Alloc          as S
import qualified Foreign.Ptr                    as S


---------------------------------------------------------------------------------------------------
-- | Relates a storage format to the Haskell type of the value
--   that is stored in that format.
class Format f where

 -- | Get the type of a value with this format.
 type Value f  

 -- | For fixed size storage formats, yield their size (length) in bytes.
 --
 --   Yields `Nothing` if this is not a fixed size format.
 --
 fixedSize  :: f -> Maybe Int

 -- | Yield the size of a value in the given format.
 --
 --   Yields `Nothing` when a collection of values is to be packed into a
 --   fixed length format, but the size of the collection does not match
 --   the format.
 --
 packedSize :: f -> Value f -> Maybe Int


---------------------------------------------------------------------------------------------------
-- | Class of storage formats that can have values packed and unpacked
--   from foreign bufferes. 
-- 
--   The methods are written using continuations to make it easier for
--   GHC to optimise its core code when packing/unpacking many fields.
--
class Format   format 
   => Packable format where

 -- | Pack a value into a buffer using the given format.
 -- 
 --   If the format contains fixed width fields and the corresponding
 --   value has too many elements, then this function returns `False`, 
 --   otherwise `True`.
 --
 pack   :: S.Ptr Word8                  -- ^ Target Buffer.
        -> format                       -- ^ Storage format.
        -> Value format                 -- ^ Value to pack.
        -> (Int -> IO (Maybe a))        -- ^ Continue, given the number of bytes written.
        -> IO (Maybe a)

 -- | Unpack a value from a buffer using the given format.
 --
 --   This is the inverse of `pack` above.
 unpack :: S.Ptr Word8                  -- ^ Source buffer.
        -> format                       -- ^ Format of buffer.
        -> ((Value format, Int) -> IO (Maybe a)) 
                                        -- ^ Continue, given the unpacked value and the 
                                        --   number of bytes read. 
        -> IO (Maybe a)


---------------------------------------------------------------------------------------------------
-- | Pack a value into a list of `Word8`.
packToList 
        :: Packable format
        => format -> Value format -> Maybe [Word8]
packToList f x
 | Just size    <- packedSize f x
 = unsafePerformIO
 $ do   buf     <- S.mallocBytes size
        mResult <- pack buf f x (\_ -> return (Just ()))
        case mResult of
         Nothing -> return Nothing
         Just _  -> do
                xs      <- mapM (S.peekByteOff buf) [0..size - 1]
                S.free buf
                return $ Just xs

 | otherwise
 = Nothing


-- | Unpack a value from a list of `Word8`.
unpackFromList
        :: Packable format
        => format -> [Word8] -> Maybe (Value format)

unpackFromList f xs
 = unsafePerformIO
 $ do   let len = length xs
        buf     <- S.mallocBytes len
        mapM_ (\(o, x) -> S.pokeByteOff buf o x)
                $ zip [0 .. len - 1] xs
        unpack buf f $ \(v, _) -> return (Just v)


---------------------------------------------------------------------------------------------------
instance (Format a, Format b) 
       => Format (a :*: b) where
 type Value (a :*: b) = Value a :*: Value b

 fixedSize  (xa :*: xb)
  = do  sa      <- fixedSize xa
        sb      <- fixedSize xb
        return  $  sa + sb
 {-# INLINE fixedSize #-}

 packedSize (fa :*: fb) (xa :*: xb)
  = do  sa      <- packedSize fa xa
        sb      <- packedSize fb xb
        return  $  sa + sb
 {-# INLINE packedSize #-}


instance (Packable fa, Packable fb) 
      =>  Packable (fa :*: fb) where

 pack   buf (fa :*: fb) (xa :*: xb) k
  =  pack buf                  fa xa $ \oa 
  -> pack (S.plusPtr buf oa)   fb xb $ \ob
  -> k (oa + ob)
 {-# INLINE pack #-}

 unpack buf (fa :*: fb) k
  =  unpack buf                fa    $ \(xa, oa)
  -> unpack (S.plusPtr buf oa) fb    $ \(xb, ob)
  -> k (xa :*: xb, oa + ob)
 {-# INLINE unpack #-}


---------------------------------------------------------------------------------------------------
-- | Fixed length list.
--
data FixList   f = FixList   f Int      deriving (Eq, Show)
instance Format f => Format (FixList   f) where
 type Value (FixList f)         = [Value f]

 fixedSize  (FixList f len)           
  = do  lenElem <- fixedSize f
        return  $ lenElem * len

 packedSize (FixList _ 0) _
  =     return 0

 packedSize (FixList f len) xs
  | length xs == len
  = do  lenElems <- mapM (packedSize f) xs
        return   $ sum lenElems

  | otherwise 
  = Nothing


-- | Variable length list.
data VarList   f = VarList   f          deriving (Eq, Show)
instance Format f => Format (VarList f) where
 type Value (VarList f)          = [Value f]

 fixedSize  (VarList _)          = Nothing

 packedSize (VarList f) xs@(x : _)
  = do  lenElem <- packedSize f x
        return  $ lenElem * length xs

 packedSize _ []
  =     return 0


---------------------------------------------------------------------------------------------------
-- | Fixed length string.
--   
--   * When packing, if the provided string is shorter than the fixed length
--     then the extra bytes are zero-filled. 
--
data FixString t = FixString t Int      deriving (Eq, Show)
instance Format (FixString ASCII)       where
 type Value (FixString ASCII)       = String
 fixedSize  (FixString ASCII len)   = Just len
 packedSize (FixString ASCII len) _ = Just len


instance Packable (FixString ASCII) where
 
  pack buf   (FixString ASCII lenField) xs k
   = do let !lenChars   = length xs
        let !lenPad     = lenField - lenChars

        if lenChars > lenField
         then return Nothing
         else do
                mapM_ (\(o, x) -> S.pokeByteOff buf o (w8 $ ord x)) 
                        $ zip [0 .. lenChars - 1] xs

                mapM_ (\o      -> S.pokeByteOff buf (lenChars + o) (0 :: Word8))
                        $ [0 .. lenPad - 1]

                k lenField
  {-# NOINLINE pack #-}

  unpack buf (FixString ASCII lenField) k
   = do 
        let load_unpackChar o
                = do    x :: Word8 <- S.peekByteOff buf o
                        return $ chr $ fromIntegral x
            {-# INLINE load_unpackChar #-}

        xs      <- mapM load_unpackChar [0 .. lenField - 1]
        let (pre, _) = break (== '\0') xs
        k (pre, lenField)
  {-# NOINLINE unpack #-}


-- | Variable length string.
data VarString t = VarString t          deriving (Eq, Show)
instance Format (VarString ASCII)       where
 type Value (VarString ASCII)       = String
 fixedSize  (VarString ASCII)       = Nothing
 packedSize (VarString ASCII) xs    = Just $ length xs


instance Packable (VarString ASCII) where

  pack buf   (VarString ASCII) xs k
   = do let !lenChars   = length xs

        mapM_ (\(o, x) -> S.pokeByteOff buf o (w8 $ ord x))
                $ zip [0 .. lenChars - 1] xs

        k lenChars
  {-# NOINLINE pack #-}

  unpack _   (VarString ASCII) _
   = return Nothing
  {-# NOINLINE unpack #-}


-- | String is encoded as 8-bit ASCII characters.
data ASCII       = ASCII                deriving (Eq, Show)


---------------------------------------------------------------------------------------------------
w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8  #-}

