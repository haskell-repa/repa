
module Data.Repa.Convert.Format.Lists 
        ( -- * Lists
          FixList(..)
        , VarList(..)

          -- * ASCII Strings
        , FixString     (..)
        , VarString     (..)
        , ASCII         (..))
where
import Data.Repa.Convert.Format.Base
import Data.Word
import Data.Char
import qualified Foreign.Storable               as S


---------------------------------------------------------------------------------------------------
-- | Fixed length list.
--
data FixList    f = FixList   f Int      deriving (Eq, Show)
instance Format f => Format (FixList   f) where
 type Value (FixList f)         
  = [Value f]

 fieldCount _ 
  = Just 1

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
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


-- | Variable length list.
data VarList   f = VarList   f          deriving (Eq, Show)
instance Format f => Format (VarList f) where
 type Value (VarList f)
        = [Value f]

 fieldCount _
  = Just 1

 fixedSize  (VarList _)
  = Nothing

 packedSize (VarList f) xs@(x : _)
  = do  lenElem <- packedSize f x
        return  $ lenElem * length xs

 packedSize _ []
  =     return 0
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


---------------------------------------------------------------------------------------------------
-- | Fixed length string.
--   
--   * When packing, if the provided string is shorter than the fixed length
--     then the extra bytes are zero-filled. 
--
data FixString t = FixString t Int      deriving (Eq, Show)
instance Format (FixString ASCII)       where
 type Value (FixString ASCII)       = String
 fieldCount _                       = Just 1
 fixedSize  (FixString ASCII len)   = Just len
 packedSize (FixString ASCII len) _ = Just len
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


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
 fieldCount _                       = Just 1
 fixedSize  (VarString ASCII)       = Nothing
 packedSize (VarString ASCII) xs    = Just $ length xs
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


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


w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8  #-}

