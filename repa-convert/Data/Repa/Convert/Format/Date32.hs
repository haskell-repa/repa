
module Data.Repa.Convert.Format.Date32
        ( YYYYsMMsDD (..)
        , DDsMMsYYYY (..))
where
import Data.Repa.Convert.Format.Base
import Data.Repa.Convert.Format.Numeric
import Data.Repa.Convert.Format.Binary
import Data.Repa.Bits.Date32                    (Date32)
import Data.Monoid
import Data.Char
import Data.Word
import qualified Data.Repa.Bits.Date32          as Date32


---------------------------------------------------------------------------------------- YYYYsMMsDD
-- | Date32 in ASCII YYYYsMMsDD format.
data YYYYsMMsDD         = YYYYsMMsDD Char       deriving (Eq, Show)
instance Format YYYYsMMsDD where
 type Value YYYYsMMsDD  = Date32
 fieldCount _           = 1
 minSize    _           = 10
 fixedSize  _           = Just 10
 packedSize _ _         = Just 10
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable YYYYsMMsDD where

 pack  (YYYYsMMsDD s) v 
  | (yy', mm', dd')        <- Date32.unpack v
  , yy  <- fromIntegral yy'
  , mm  <- fromIntegral mm'
  , dd  <- fromIntegral dd'
  =        pack (IntAsc0 4) yy
        <> pack Word8be     (cw8 s)
        <> pack (IntAsc0 2) mm
        <> pack Word8be     (cw8 s)
        <> pack (IntAsc0 4) dd
 {-# INLINE pack #-}

 unpack buf len (YYYYsMMsDD s) k
  = do  r       <- Date32.loadYYYYsMMsDD (fromIntegral $ ord s) buf len
        case r of
         Just (d, o)    -> k (d, o)
         Nothing        -> return Nothing
 {-# INLINE unpack #-}


---------------------------------------------------------------------------------------- DDsMMsYYYY
-- | Date32 in ASCII DDsMMsYYYY format.
data DDsMMsYYYY         = DDsMMsYYYY Char       deriving (Eq, Show)
instance Format DDsMMsYYYY where
 type Value DDsMMsYYYY  = Date32
 fieldCount _           = 1
 minSize    _           = 10
 fixedSize  _           = Just 10
 packedSize _ _         = Just 10
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable DDsMMsYYYY where

 pack   (DDsMMsYYYY s) v
  | (yy', mm', dd')        <- Date32.unpack v
  , yy  <- fromIntegral yy'
  , mm  <- fromIntegral mm'
  , dd  <- fromIntegral dd'
  =        pack (IntAsc0 2) dd
        <> pack Word8be     (cw8 s)
        <> pack (IntAsc0 2) mm
        <> pack Word8be     (cw8 s)
        <> pack (IntAsc0 4) yy
 {-# INLINE pack #-}

 unpack buf len (DDsMMsYYYY s) k
  = do  r       <- Date32.loadDDsMMsYYYY (fromIntegral $ ord s) buf len
        case r of
         Just (d, o)    -> k (d, o)
         Nothing        -> return Nothing
 {-# INLINE unpack #-}


---------------------------------------------------------------------------------------------------
cw8 :: Char -> Word8
cw8 c = fromIntegral $ ord c
{-# INLINE cw8 #-}




