{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Repa.Convert.Format.Fields where
import Data.Repa.Convert.Format.Base
import Data.Repa.Scalar.Product
#include "repa-convert.h"


instance Format () where
 type Value () = ()
 fieldCount _   = 0
 minSize    _   = 0
 fixedSize  _   = return 0
 packedSize _ _ = return 0
 {-# INLINE_INNER minSize    #-}
 {-# INLINE_INNER fieldCount #-}
 {-# INLINE_INNER fixedSize  #-}
 {-# INLINE_INNER packedSize #-}


instance Packable () where
 pack   _       = mempty
 unpack _       = return ()
 {-# INLINE_INNER pack   #-} 
 {-# INLINE_INNER unpack #-}


-- | Formatting fields.
instance (Format a, Format b) 
       => Format (a :*: b) where

 type Value (a :*: b)
  = Value a :*: Value b

 fieldCount (fa :*: fb)
  = fieldCount fa + fieldCount fb

 minSize    (fa :*: fb)
  = minSize fa + minSize fb

 fixedSize  (fa :*: fb)
  = do  sa      <- fixedSize fa
        sb      <- fixedSize fb
        return  $  sa + sb

 packedSize (fa :*: fb) (xa :*: xb)
  = do  sa      <- packedSize fa xa
        sb      <- packedSize fb xb
        return  $  sa + sb

 {-# INLINE_INNER minSize #-}
 {-# INLINE_INNER fieldCount #-}
 {-# INLINE_INNER fixedSize #-}
 {-# INLINE_INNER packedSize #-}

