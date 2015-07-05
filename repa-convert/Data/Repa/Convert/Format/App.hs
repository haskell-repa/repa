
module Data.Repa.Convert.Format.App
        (App (..))
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Repa.Convert.Internal.Packer
import Data.Repa.Scalar.Product
import Data.Monoid
import Prelude hiding (fail)
#include "repa-convert.h"


-- | Append fields without separators.
data App f = App f         


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
 {-# NOINLINE minSize    #-}

 fieldCount (App (_f1 :*: fs))
  = 1 + fieldCount (App fs)
 {-# NOINLINE fieldCount #-}

 fixedSize  (App (f1 :*: fs))
  = do  s1      <- fixedSize f1
        ss      <- fixedSize (App fs)
        return  $ s1 + ss
 {-# NOINLINE fixedSize  #-}

 packedSize (App (f1 :*: fs)) (x1 :*: xs)
  = do  s1      <- packedSize f1       x1
        ss      <- packedSize (App fs) xs
        return  $ s1 + ss
 {-# NOINLINE packedSize #-}


instance Packable (App ()) where
 packer _f _v start _fails eat
        = eat start
 {-# INLINE packer #-}
 
 unpacker _f start _end _stop _fails eat
        = eat start ()
 {-# INLINE unpacker #-}


instance ( Packable f1, Packable (App fs)
         , Value (App fs) ~ Value fs)
      => Packable (App (f1 :*: fs)) where

 pack    (App (f1 :*: fs)) (x1 :*: xs)
  =     pack f1 x1 <> pack (App fs) xs
 {-# NOINLINE pack #-}

 packer  f v
  = fromPacker $ pack f v
 {-# INLINE packer #-}

 unpacker  (App (f1 :*: fs)) start end stop fail eat
  = unpacker     f1       start    end stop fail $ \start_x1 x1
     -> unpacker (App fs) start_x1 end stop fail $ \start_xs xs
         -> eat start_xs (x1 :*: xs) 
 {-# INLINE unpack #-}


