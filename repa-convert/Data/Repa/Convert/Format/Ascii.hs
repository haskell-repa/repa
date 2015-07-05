
module Data.Repa.Convert.Format.Ascii
        (FormatAscii (..))
where
import Data.Repa.Convert.Format.Numeric
import Data.Repa.Convert.Format.String
import Data.Repa.Convert.Format.Date32
import Data.Repa.Convert.Format.Tup
import Data.Repa.Scalar.Date32                  (Date32)
import Data.Repa.Scalar.Product
#include "repa-convert.h"


-- | Class of types that can be formatted in some default human readable
--   ASCII way.
class FormatAscii a where
 -- | The format for values of this type.
 type FormatAscii' a 

 -- | Get the standard ASCII format for a value.
 --
 --   The element value itself is not demanded.
 --
 formatAscii :: a -> FormatAscii' a

data Plain a 
        = Plain a


-- | Empty tuples produce no output.
instance FormatAscii () where
 type FormatAscii' ()     = ()
 formatAscii _            = ()
 {-# INLINE formatAscii #-}


instance ( FormatAscii t1
         , FormatAscii (Plain ts))
        => FormatAscii (t1 :*: ts) where

 type FormatAscii' (t1 :*: ts)        
  = Tup      (FormatAscii' t1 :*: FormatAscii' (Plain ts))

 formatAscii _            
  = let -- The values of these type proxies should never be demanded.
        (x1_proxy :: t1)  = error "repa-convert: formatAscii proxy"
        (xs_proxy :: ts)  = error "repa-convert: formatAscii proxy"
    in  Tup (formatAscii x1_proxy  :*: formatAscii  (Plain xs_proxy))
 {-# NOINLINE formatAscii #-}


instance FormatAscii (Plain ()) where
 type FormatAscii'   (Plain ())          = ()
 formatAscii         (Plain _)           = ()
 {-# INLINE formatAscii #-}


instance (FormatAscii t1, FormatAscii (Plain ts))
      => FormatAscii (Plain (t1 :*: ts)) where
 type FormatAscii'   (Plain (t1 :*: ts)) 
  = FormatAscii' t1 :*: FormatAscii' (Plain ts)

 formatAscii _
  = let -- The values of these type proxies should never be demanded.
        (x1_proxy :: t1)  = error "repa-convert: formatAscii proxy"
        (xs_proxy :: ts)  = error "repa-convert: formatAscii proxy"
    in  formatAscii  x1_proxy :*: formatAscii (Plain xs_proxy)
 {-# NOINLINE formatAscii #-}
 
 
-- | Ints are formated in base-10.
instance FormatAscii  Int where
 type FormatAscii' Int    = IntAsc
 formatAscii _            = IntAsc
 {-# INLINE formatAscii #-}


-- | Doubles are formatted as base-10 decimal.
instance FormatAscii  Double where
 type FormatAscii' Double = DoubleAsc
 formatAscii _            = DoubleAsc
 {-# INLINE formatAscii #-}


-- | Strings are formatted with double quotes and back-slash escaping
--   of special characters.
instance FormatAscii  String where
 type FormatAscii' String = VarCharString
 formatAscii _            = VarCharString
 {-# INLINE formatAscii #-}


-- | Dates are formatted as YYYY-MM-DD.
instance FormatAscii  Date32 where
 type FormatAscii' Date32 = YYYYsMMsDD
 formatAscii _            = YYYYsMMsDD '-'
 {-# INLINE formatAscii #-}

