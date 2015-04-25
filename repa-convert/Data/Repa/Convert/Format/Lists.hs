
module Data.Repa.Convert.Format.Lists 
        ( -- * Lists
          FixList(..)
        , VarList(..)

          -- * ASCII Strings
        , FixAsc (..)
        , VarAsc (..))
where
import Data.Repa.Convert.Format.Base
import Data.Word
import Data.Char
import qualified Foreign.Storable               as S


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
 
  pack buf   (FixAsc lenField) xs k
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

  unpack buf _ (FixAsc lenField) k
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

  pack buf       VarAsc xs k
   = do let !lenChars   = length xs

        mapM_ (\(o, x) -> S.pokeByteOff buf o (w8 $ ord x))
                $ zip [0 .. lenChars - 1] xs

        k lenChars
  {-# NOINLINE pack #-}

  unpack buf len VarAsc k
   = do 
        -- We don't locally know what the stopping point should
        -- for the string, so just decode all the way to the end.
        -- If the caller knows the field should be shorter then
        -- it should pass in a shorter length.
        let load_unpackChar o
                = do    x :: Word8      <- S.peekByteOff buf o
                        return $ chr $ fromIntegral x
            {-# INLINE load_unpackChar #-}
                        
        xs      <- mapM load_unpackChar [0 .. len - 1]
        k (xs, len)
  {-# NOINLINE unpack #-}


w8  :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8  #-}

