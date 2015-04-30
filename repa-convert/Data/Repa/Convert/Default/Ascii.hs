
module Data.Repa.Convert.Default.Ascii
        (FormatAscii (..))
where
import Data.Repa.Bits.Date32                    (Date32)
import Data.Repa.Convert.Format.Numeric
import Data.Repa.Convert.Format.Lists
import Data.Repa.Convert.Format.Date32
import Data.Repa.Convert.Format.Sep
import Data.Repa.Product


-- | Class of types that can be formatted in some default human readable
--   ASCII way.
class FormatAscii a where
 -- | The format for this type.
 type FormatAscii' a 
 formatAscii :: a -> FormatAscii' a

data Plain a 
        = Plain a


instance FormatAscii () where
 type FormatAscii' ()     = ()
 formatAscii _            = ()
 {-# INLINE formatAscii #-}


instance ( FormatAscii t1
         , FormatAscii (Plain ts))
        => FormatAscii (t1 :*: ts) where
 type FormatAscii' (t1 :*: ts)        
  = Sep      (FormatAscii' t1 :*: FormatAscii' (Plain ts))

 formatAscii (x1 :*: xs)            
  = Sep '\t' (formatAscii x1  :*: formatAscii  (Plain xs))
 {-# INLINE formatAscii #-}


instance FormatAscii (Plain ()) where
 type FormatAscii'   (Plain ())          = ()
 formatAscii         (Plain _)           = ()
 {-# INLINE formatAscii #-}


instance (FormatAscii t1, FormatAscii (Plain ts))
      => FormatAscii (Plain (t1 :*: ts)) where
 type FormatAscii'   (Plain (t1 :*: ts)) = FormatAscii' t1 :*: FormatAscii' (Plain ts)
 formatAscii         (Plain (x1 :*: xs)) = formatAscii  x1 :*: formatAscii  (Plain xs)

 
instance FormatAscii  Int where
 type FormatAscii' Int    = IntAsc
 formatAscii _            = IntAsc
 {-# INLINE formatAscii #-}


instance FormatAscii  Double where
 type FormatAscii' Double = DoubleAsc
 formatAscii _            = DoubleAsc
 {-# INLINE formatAscii #-}


instance FormatAscii  String where
 type FormatAscii' String = VarAsc
 formatAscii _            = VarAsc
 {-# INLINE formatAscii #-}


instance FormatAscii  Date32 where
 type FormatAscii' Date32 = YYYYsMMsDD
 formatAscii _            = YYYYsMMsDD '-'
 {-# INLINE formatAscii #-}


