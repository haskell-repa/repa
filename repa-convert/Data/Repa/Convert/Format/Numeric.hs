
module Data.Repa.Convert.Format.Numeric
        ( IntAsc                (..)
        , IntAsc0               (..)
        , DoubleAsc             (..)
        , DoubleFixedPack       (..))
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Repa.Convert.Format.Lists
import GHC.Exts
import Data.Word
import qualified Data.Repa.Scalar.Int           as S
import qualified Data.Repa.Scalar.Double        as S
import qualified Foreign.ForeignPtr             as F
import qualified Foreign.Marshal.Utils          as F
import qualified Foreign.Ptr                    as F
import Prelude hiding (fail)
#include "repa-convert.h"


------------------------------------------------------------------------------------------- IntAsc
-- | Human-readable ASCII Integer.
data IntAsc     = IntAsc        deriving (Eq, Show)
instance Format IntAsc where
 type Value IntAsc      = Int

 fieldCount _           = 1
 {-# INLINE minSize    #-}

 minSize    _           = 1
 {-# INLINE fieldCount #-}

 fixedSize  _           = Nothing
 {-# INLINE fixedSize  #-}

 -- Max length of a pretty printed 64-bit Int is 20 bytes including sign.
 packedSize _ _         = Just 20               
 {-# INLINE packedSize #-}


instance Packable IntAsc where

 -- ISSUE #43: Avoid intermediate lists when packing Ints and Strings.
 packer IntAsc v buf k
  = packer VarCharList (show v) buf k
 {-# INLINE packer #-}

 unpacker IntAsc start end _stop fail eat
  = let !len = I# (minusAddr# end start) in 
    if len > 0
     then do
       r       <- S.loadInt (pw8 start) len
       case r of
        Just (n, I# o)  -> eat (plusAddr# start o) n
        Nothing         -> fail
     else fail
 {-# INLINE unpacker #-}


------------------------------------------------------------------------------------------- IntAsc
-- | Human-readable ASCII integer, with leading zeros.
data IntAsc0    = IntAsc0 Int   deriving (Eq, Show)
instance Format IntAsc0 where
 type Value IntAsc0     = Int
 fieldCount _           = 1
 minSize    _           = 1
 fixedSize  _           = Nothing

 -- Max length of a pretty printed 64-bit Int is 20 bytes including sign.
 packedSize (IntAsc0 n) _ = Just (n + 20)
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable IntAsc0 where

 -- ISSUE #43: Avoid intermediate lists when packing Ints and Strings.
 packer (IntAsc0 n) v start k
  = let s       = show v
        s'      = replicate (n - length s) '0' ++ s
    in  packer VarCharList s' start k
 {-# INLINE packer #-}

 unpacker (IntAsc0 _) start end _stop fail eat
  = let !len = I# (minusAddr# end start) in
    if  len > 0
     then do
        r       <- S.loadInt (pw8 start) len
        case r of
         Just (n, I# o) -> eat (plusAddr# start o) n
         Nothing        -> fail
     else fail
 {-# INLINE unpacker #-}


----------------------------------------------------------------------------------------- DoubleAsc
-- | Human-readable ASCII Double.
data DoubleAsc  = DoubleAsc     deriving (Eq, Show)
instance Format DoubleAsc where
 type Value DoubleAsc   = Double
 fieldCount _           = 1
 minSize    _           = 1
 fixedSize  _           = Nothing

 -- Max length of a pretty-printed 64-bit double is 24 bytes.
 packedSize _ _         = Just 24
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable DoubleAsc where

 packer  DoubleAsc v start k
  = do  (fptr, len)  <- S.storeDoubleShortest v
        F.withForeignPtr fptr $ \ptr
         -> F.copyBytes start ptr len
        k (F.plusPtr start len)
 {-# INLINE packer   #-}

 unpacker DoubleAsc start end _stop fail eat
  = let !len = I# (minusAddr# end start) in
    if len > 0
      then do
        (v, I# o)  <- S.loadDouble (pw8 start) len
        eat (plusAddr# start o) v
      else fail
 {-# INLINE unpacker #-}


-------------------------------------------------------------------------------- DoubleFixedPack
-- | Human-readable ASCII Double.
-- 
--   When packing we use a fixed number of zeros after the decimal
--   point, though when unpacking we allow a greater precision.
--
data DoubleFixedPack    = DoubleFixedPack Int   deriving (Eq, Show)
instance Format DoubleFixedPack where
 type Value DoubleFixedPack = Double
 fieldCount _           = 1
 minSize    _           = 1
 fixedSize  _           = Nothing

 -- Max length of a pretty-printed 64-bit double is 24 bytes.
 packedSize (DoubleFixedPack prec) _         
                        = Just (24 + prec)
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable DoubleFixedPack where

 packer   (DoubleFixedPack prec) v start k
  = do  (fptr, len)  <- S.storeDoubleFixed prec v
        F.withForeignPtr fptr $ \ptr
         -> F.copyBytes start ptr len
        k (F.plusPtr start len)
 {-# INLINE packer #-}

 unpacker (DoubleFixedPack _) start end _stop fail eat
  = let !len = I# (minusAddr# end start) in
    if len > 0
     then do
       (v, I# o)  <- S.loadDouble (pw8 start) len
       eat (plusAddr# start o) v
     else fail
 {-# INLINE unpacker #-}


pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}

