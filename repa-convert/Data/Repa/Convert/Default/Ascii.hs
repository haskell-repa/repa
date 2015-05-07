
module Data.Repa.Convert.Default.Ascii
        (FormatAscii (..))
where
import Data.Repa.Bits.Date32                    (Date32)
import Data.Repa.Convert.Format.Numeric
import Data.Repa.Convert.Format.Lists
import Data.Repa.Convert.Format.Date32
import Data.Repa.Convert.Format.Row
import Data.Repa.Product


-- | Class of types that can be formatted in some default human readable
--   ASCII way.
class FormatAscii a where
 -- | The format for this type.
 type FormatAscii' a 

 -- | Get the format for an element.
 --
 --   The element value itself is not demanded.
 --
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
  = Row      (FormatAscii' t1 :*: FormatAscii' (Plain ts))

 formatAscii _            
  = let (x1_proxy :: t1)  = error "repa-convert: formatAscii proxy"
        (xs_proxy :: ts)  = error "repa-convert: formatAscii proxy"
    in  Row '\t' (formatAscii x1_proxy  :*: formatAscii  (Plain xs_proxy))
 {-# INLINE formatAscii #-}


instance FormatAscii (Plain ()) where
 type FormatAscii'   (Plain ())          = ()
 formatAscii         (Plain _)           = ()
 {-# INLINE formatAscii #-}


instance (FormatAscii t1, FormatAscii (Plain ts))
      => FormatAscii (Plain (t1 :*: ts)) where
 type FormatAscii'   (Plain (t1 :*: ts)) 
  = FormatAscii' t1 :*: FormatAscii' (Plain ts)

 formatAscii         _
  = let (x1_proxy :: t1)  = error "repa-convert: formatAscii proxy"
        (xs_proxy :: ts)  = error "repa-convert: formatAscii proxy"
    in  formatAscii  x1_proxy :*: formatAscii (Plain xs_proxy)

 
instance FormatAscii  Int where
 type FormatAscii' Int    = IntAsc
 formatAscii _            = IntAsc
 {-# INLINE formatAscii #-}


instance FormatAscii  Double where
 type FormatAscii' Double = DoubleAsc
 formatAscii _            = DoubleAsc
 {-# INLINE formatAscii #-}


instance FormatAscii  String where
 type FormatAscii' String = VarString
 formatAscii _            = VarString
 {-# INLINE formatAscii #-}


instance FormatAscii  Date32 where
 type FormatAscii' Date32 = YYYYsMMsDD
 formatAscii _            = YYYYsMMsDD '-'
 {-# INLINE formatAscii #-}


