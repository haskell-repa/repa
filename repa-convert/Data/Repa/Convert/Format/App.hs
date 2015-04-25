
module Data.Repa.Convert.Format.App
        (App (..))
where
import Data.Repa.Convert.Format.Base
import Data.Repa.Product
import qualified Foreign.Ptr                    as S


-- | Append fields without separators.
data App f
        = App f         
        deriving Show


instance Format (App ()) where
 type Value (App ())    = ()
 fieldCount (App ())    = 0
 minSize    (App ())    = 0
 fixedSize  (App ())    = return 0
 packedSize (App ()) () = return 0
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance ( Format f1, Format (App fs)
         , Value (App fs) ~ Value fs)
      =>  Format (App (f1 :*: fs)) where
 type Value (App (f1 :*: fs))   
        = Value f1 :*: Value fs

 minSize    (App (f1  :*: fs))
  = minSize f1 + minSize (App fs)

 fieldCount (App (_f1 :*: fs))
  = 1 + fieldCount (App fs)

 fixedSize  (App (f1 :*: fs))
  = do  s1      <- fixedSize f1
        ss      <- fixedSize (App fs)
        return  $ s1 + ss

 packedSize (App (f1 :*: fs)) (x1 :*: xs)
  = do  s1      <- packedSize f1       x1
        ss      <- packedSize (App fs) xs
        return  $ s1 + ss

 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable (App ()) where
  pack   _buf _fmt _val k = k 0
  unpack _buf _len _fmt k = k ((), 0)
  {-# INLINE pack   #-}
  {-# INLINE unpack #-}


instance ( Packable f1, Packable (App fs)
         , Value (App fs) ~ Value fs)
      => Packable (App (f1 :*: fs)) where

 pack    !buf (App (f1 :*: fs)) (x1 :*: xs) k
  =  pack  buf                    f1  x1 $ \o1
  -> pack (S.plusPtr buf o1) (App fs) xs $ \os
  -> k (o1 + os)

 unpack  !buf !len (App (f1 :*: fs)) k
  =  unpack  buf                len       f1        $ \(x1, o1)
  -> unpack (S.plusPtr buf o1) (len - o1) (App fs)  $ \(xs, os)
  -> k (x1 :*: xs, o1 + os)

 {-# INLINE pack #-}
 {-# INLINE unpack #-}

