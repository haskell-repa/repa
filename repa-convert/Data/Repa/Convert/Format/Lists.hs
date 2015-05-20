
module Data.Repa.Convert.Format.Lists 
        ( -- * Lists
          FixList(..)
        , VarList(..)

          -- * ASCII Strings
        , FixAsc (..)
        , VarAsc (..)
        , VarString (..))
where
import Data.Repa.Convert.Format.Binary
import Data.Repa.Convert.Format.Base
import Data.Monoid
import Data.Word
import Data.Char
import qualified Foreign.Storable               as S
import qualified Foreign.Ptr                    as S
import Prelude hiding (fail)


---------------------------------------------------------------------------------------------------
-- | Fixed length list.
data FixList    f = FixList   f Int      deriving (Eq, Show)
instance Format f => Format (FixList   f) where
 type Value (FixList f)         
  = [Value f]

 fieldCount _  
  = 1

 minSize    (FixList f len)
  = minSize f * len


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
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


-- | Variable length list.
data VarList   f = VarList   f          deriving (Eq, Show)
instance Format f => Format (VarList f) where
 type Value (VarList f)
        = [Value f]

 fieldCount _
  = 1

 minSize _ 
  = 0

 fixedSize  (VarList _)
  = Nothing

 packedSize (VarList f) xs@(x : _)
  = do  lenElem <- packedSize f x
        return  $ lenElem * length xs

 packedSize _ []
  =     return 0
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


---------------------------------------------------------------------------------------------------
-- | Fixed length string.
--   
--   * When packing, if the provided string is shorter than the fixed length
--     then the extra bytes are zero-filled. 
--
data FixAsc     = FixAsc Int            deriving (Eq, Show)
instance Format FixAsc where
 type Value (FixAsc)            = String
 fieldCount _                   = 1
 minSize    (FixAsc len)        = len
 fixedSize  (FixAsc len)        = Just len
 packedSize (FixAsc len) _      = Just len
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable FixAsc where
 
  pack (FixAsc lenField) xs 
   =  Packer $ \buf k
   -> do let !lenChars   = length xs
         let !lenPad     = lenField - lenChars

         if lenChars > lenField
          then return Nothing
          else do
                mapM_ (\(o, x) -> S.pokeByteOff buf o (w8 $ ord x)) 
                        $ zip [0 .. lenChars - 1] xs

                mapM_ (\o      -> S.pokeByteOff buf (lenChars + o) (0 :: Word8))
                        $ [0 .. lenPad - 1]

                k (S.plusPtr buf lenField)
  {-# NOINLINE pack #-}

  unpack (FixAsc lenField)
   =  Unpacker $ \start _end _fail eat
   -> do 
        let load_unpackChar o
                = do    x :: Word8 <- S.peekByteOff start o
                        return $ chr $ fromIntegral x
            {-# INLINE load_unpackChar #-}

        xs      <- mapM load_unpackChar [0 .. lenField - 1]
        let (pre, _) = break (== '\0') xs
        eat (S.plusPtr start lenField)
            pre
  {-# NOINLINE unpack #-}


---------------------------------------------------------------------------------------------------
-- | Variable length raw string (with no quotes).
data VarAsc = VarAsc            deriving (Eq, Show)
instance Format (VarAsc)        where
 type Value VarAsc              = String
 fieldCount _                   = 1
 minSize    _                   = 0
 fixedSize  VarAsc              = Nothing
 packedSize VarAsc xs           = Just $ length xs
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable VarAsc where

  pack VarAsc xx
   = case xx of
        []       -> mempty
        (x : xs) -> pack Word8be (w8 $ ord x) <> pack VarAsc xs
  {-# NOINLINE pack #-}

  unpack VarAsc 
   = Unpacker $ \start end _fail eat
   -> do
        -- We don't locally know what the stopping point should
        -- for the string, so just decode all the way to the end.
        -- If the caller knows the field should be shorter then
        -- it should pass in a shorter length.
        let load_unpackChar o
                = do    x :: Word8  <- S.peekByteOff start o
                        return $ chr $ fromIntegral x
            {-# INLINE load_unpackChar #-}
                        
        let !len = S.minusPtr end start
        xs       <- mapM load_unpackChar [0 .. len - 1]
        eat (S.plusPtr start len) xs
  {-# NOINLINE unpack #-}


---------------------------------------------------------------------------------------------------
-- | Variable length string in double quotes, 
--   and standard backslash encoding of special characters.
data VarString = VarString      deriving (Eq, Show)
instance Format VarString       where
 type Value VarString           = String
 fieldCount _                   = 1
 minSize    _                   = 2
 fixedSize  _                   = Nothing
 packedSize VarString xs        
  = Just $ length $ show xs     
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable VarString where

 -- ISSUE #43: Avoid intermediate lists when packing Ints and Strings.
 pack VarString xx
  =  pack VarAsc (show xx)
 {-# INLINE pack #-}

 unpack VarString
  =  Unpacker $ \start end fail eat
  -> do r       <- unpackString start end
        case r of
         Nothing            -> fail
         Just (start', str) -> eat start' str
 {-# INLINE unpack #-}


-- | Unpack a string from the given buffer.
unpackString 
        :: S.Ptr Word8    -- ^ First byte in buffer.
        -> S.Ptr Word8    -- ^ First byte after buffer.
        -> IO (Maybe (S.Ptr Word8, [Char]))

unpackString start end
 = open start
 where
        -- Accept the open quotes.
        open !ptr
         | ptr >= end
         = return $ Nothing

         | otherwise
         = do   w :: Word8  <- S.peek ptr
                let !ptr'   =  S.plusPtr ptr 1 
                case chr $ fromIntegral w of
                 '"'    -> go_body ptr' []
                 _      -> return Nothing

        -- Handle the next character in the string.
        go_body !ptr !acc
         | ptr >= end 
         = return $ Just (ptr, reverse acc)

         | otherwise
         = do   w :: Word8  <- S.peek ptr
                let !ptr'   =  S.plusPtr ptr 1
                case chr $ fromIntegral w of
                 '"'    -> return $ Just (ptr', reverse acc)
                 '\\'   -> go_escape ptr' acc
                 c      -> go_body   ptr' (c : acc)

        -- Handle escaped character.
        -- The previous character was a '\\'
        go_escape !ptr !acc
         | ptr >= end
         = return Nothing

         | otherwise
         = do   w :: Word8  <- S.peek ptr
                let ptr'    =  S.plusPtr ptr 1
                case chr $ fromIntegral w of
                 'a'    -> go_body ptr' ('\a' : acc)
                 'b'    -> go_body ptr' ('\b' : acc)
                 'f'    -> go_body ptr' ('\f' : acc)
                 'n'    -> go_body ptr' ('\n' : acc)
                 'r'    -> go_body ptr' ('\r' : acc)
                 't'    -> go_body ptr' ('\t' : acc)
                 'v'    -> go_body ptr' ('\v' : acc)
                 '\\'   -> go_body ptr' ('\\' : acc)
                 '"'    -> go_body ptr' ('"'  : acc)
                 _      -> return Nothing
{-# NOINLINE unpackString #-}


---------------------------------------------------------------------------------------------------
w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8  #-}

