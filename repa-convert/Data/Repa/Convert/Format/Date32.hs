
module Data.Repa.Convert.Format.Date32
        ( YYYYsMMsDD (..)
        , DDsMMsYYYY (..))
where
import Data.Repa.Convert.Internal.Packable
import Data.Repa.Convert.Internal.Packer
import Data.Repa.Convert.Format.Numeric
import Data.Repa.Convert.Format.Binary
import Data.Monoid
import Data.Char
import Data.Word
import GHC.Exts
import Data.Repa.Scalar.Date32                  (Date32)
import qualified Data.Repa.Scalar.Date32        as Date32
import Prelude hiding (fail)
#include "repa-convert.h"


---------------------------------------------------------------------------------------- YYYYsMMsDD
-- | Human readable ASCII date in YYYYsMMsDD format.
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

 pack  (YYYYsMMsDD s) !v
  = let (yy', mm', dd') = Date32.unpack v
        !yy     = fromIntegral yy'
        !mm     = fromIntegral mm'
        !dd     = fromIntegral dd'
    in     pack (IntAsc0 4) yy
        <> pack Word8be     (cw8 s)
        <> pack (IntAsc0 2) mm
        <> pack Word8be     (cw8 s)
        <> pack (IntAsc0 2) dd
 {-# INLINE pack #-}

 packer f v
  = fromPacker (pack f v)
 {-# INLINE packer #-}

 unpacker (YYYYsMMsDD s) start end _stop fail eat
  = do  let len = I# (minusAddr# end start)
        r       <- Date32.loadYYYYsMMsDD (fromIntegral $ ord s) (pw8 start) len
        case r of
         Just (d, I# o) -> eat (plusAddr# start o) d
         Nothing        -> fail
 {-# INLINE unpack #-}


---------------------------------------------------------------------------------------- DDsMMsYYYY
-- | Human readable ASCII date in DDsMMsYYYY format.
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

 pack   (DDsMMsYYYY s) !v
  = let (yy', mm', dd') = Date32.unpack v
        !yy     = fromIntegral yy'
        !mm     = fromIntegral mm'
        !dd     = fromIntegral dd'
    in     pack (IntAsc0 2) dd
        <> pack Word8be     (cw8 s)
        <> pack (IntAsc0 2) mm
        <> pack Word8be     (cw8 s)
        <> pack (IntAsc0 4) yy
 {-# INLINE pack #-}

 packer f v
  = fromPacker (pack f v)
 {-# INLINE packer #-}

 unpacker (DDsMMsYYYY s) start end _stop fail eat
  = do
        let len = I# (minusAddr# end start)
        r       <- Date32.loadDDsMMsYYYY (fromIntegral $ ord s) (pw8 start) len
        case r of
         Just (d, I# o)    -> eat (plusAddr# start o) d
         Nothing           -> fail
 {-# INLINE unpack #-}


---------------------------------------------------------------------------------------------------
cw8 :: Char -> Word8
cw8 c = fromIntegral $ ord c
{-# INLINE cw8 #-}

pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}


