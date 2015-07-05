{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Repa.Convert.Format.Fields where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Repa.Scalar.Product
#include "repa-convert.h"


---------------------------------------------------------------------------------------------------
instance Format () where
 type Value () = ()
 fieldCount _   = 0
 minSize    _   = 0
 fixedSize  _   = return 0
 packedSize _ _ = return 0
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable () where
 packer  _f _v dst _fails k
        = k dst
 {-# INLINE packer #-}

 unpacker _f start _end _stop _fail eat
        = eat start ()
 {-# INLINE unpack #-}


---------------------------------------------------------------------------------------------------
-- | Formatting fields.
instance (Format a, Format b) 
       => Format (a :*: b) where

 type Value (a :*: b)
  = Value a :*: Value b

 fieldCount (fa :*: fb)
  = fieldCount fa + fieldCount fb
 {-# NOINLINE fieldCount #-}

 minSize    (fa :*: fb)
  = minSize fa + minSize fb
 {-# NOINLINE minSize #-}

 fixedSize  (fa :*: fb)
  = do  sa      <- fixedSize fa
        sb      <- fixedSize fb
        return  $  sa + sb
 {-# NOINLINE fixedSize #-}

 packedSize (fa :*: fb) (xa :*: xb)
  = do  sa      <- packedSize fa xa
        sb      <- packedSize fb xb
        return  $  sa + sb
 {-# NOINLINE packedSize #-}

