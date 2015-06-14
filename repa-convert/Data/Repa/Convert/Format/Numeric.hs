
module Data.Repa.Convert.Format.Numeric
        ( IntAsc                (..)
        , IntAsc0               (..)
        , DoubleAsc             (..)
        , DoubleFixedPack       (..))
where
import Data.Repa.Convert.Format.Base
import Data.Repa.Convert.Format.Lists
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
 minSize    _           = 1
 fixedSize  _           = Nothing

 -- Max length of a pretty printed 64-bit Int is 20 bytes including sign.
 packedSize _ _         = Just 20               
 {-# INLINE_INNER minSize    #-}
 {-# INLINE_INNER fieldCount #-}
 {-# INLINE_INNER fixedSize  #-}
 {-# INLINE_INNER packedSize #-}


instance Packable IntAsc where

 -- ISSUE #43: Avoid intermediate lists when packing Ints and Strings.
 pack IntAsc v
  = pack VarAsc (show v)
 {-# INLINE_INNER pack #-}

 unpack IntAsc 
  =  Unpacker $ \start end _stop fail eat
  -> let !len = F.minusPtr end start in 
     if len > 0
        then do
          r       <- S.loadInt start len
          case r of
           Just (n, o)  -> eat (F.plusPtr start o) n
           Nothing      -> fail
        else fail
 {-# INLINE_INNER unpack #-}


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
 {-# INLINE_INNER minSize    #-}
 {-# INLINE_INNER fieldCount #-}
 {-# INLINE_INNER fixedSize  #-}
 {-# INLINE_INNER packedSize #-}


instance Packable IntAsc0 where

 -- ISSUE #43: Avoid intermediate lists when packing Ints and Strings.
 pack   (IntAsc0 n) v 
  = let s       = show v
        s'      = replicate (n - length s) '0' ++ s
    in  pack VarAsc s'
 {-# INLINE_INNER pack #-}

 unpack (IntAsc0 _)
  =  Unpacker $ \start end _stop fail eat
  -> let !len = F.minusPtr end start in
     if len > 0
      then do
        r       <- S.loadInt start len
        case r of
         Just (n, o)    -> eat (F.plusPtr start o) n
         Nothing        -> fail
      else fail
 {-# INLINE_INNER unpack #-}


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
 {-# INLINE_INNER minSize    #-}
 {-# INLINE_INNER fieldCount #-}
 {-# INLINE_INNER fixedSize  #-}
 {-# INLINE_INNER packedSize #-}


instance Packable DoubleAsc where

 pack   DoubleAsc v 
  =  Packer $ \buf k
  -> do (fptr, len)  <- S.storeDoubleShortest v
        F.withForeignPtr fptr $ \ptr
         -> F.copyBytes buf ptr len
        k (F.plusPtr buf len)
 {-# INLINE_INNER pack   #-}

 unpack DoubleAsc 
  =  Unpacker $ \start end _stop fail eat
  -> let !len = F.minusPtr end start in
     if len > 0
      then do
        (v, o)  <- S.loadDouble start len
        eat (F.plusPtr start o) v
      else fail
 {-# INLINE_INNER unpack #-}


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
 {-# INLINE_INNER minSize    #-}
 {-# INLINE_INNER fieldCount #-}
 {-# INLINE_INNER fixedSize  #-}
 {-# INLINE_INNER packedSize #-}


instance Packable DoubleFixedPack where

 pack   (DoubleFixedPack prec) v 
  =  Packer $ \buf k
  -> do (fptr, len)  <- S.storeDoubleFixed prec v
        F.withForeignPtr fptr $ \ptr
         -> F.copyBytes buf ptr len
        k (F.plusPtr buf len)
 {-# INLINE_INNER pack   #-}

 unpack (DoubleFixedPack _)
  =  Unpacker $ \start end _stop fail eat
  -> let !len = F.minusPtr end start in
     if len > 0
      then do
        (v, o)  <- S.loadDouble start len
        eat (F.plusPtr start o) v
      else fail
 {-# INLINE_INNER unpack #-}


