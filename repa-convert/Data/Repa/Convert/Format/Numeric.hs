
module Data.Repa.Convert.Format.Numeric
        ( IntAsc    (..)
        , DoubleAsc (..))
where
import Data.Repa.Convert.Format.Base
import Data.Repa.Convert.Format.Lists
import Data.Repa.Convert.Numeric
import qualified Foreign.ForeignPtr             as F
import qualified Foreign.Marshal.Utils          as F


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
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable IntAsc where

 unpack buf len IntAsc k 
  = do  r       <- loadInt buf len
        case r of
          Just (n, o)     -> k (n, o)
          _               -> return Nothing
 {-# INLINE unpack #-}

 -- TODO: This is very slow. Avoid going via lists.
 pack   buf IntAsc v k
  = pack buf VarAsc (show v) k
 {-# INLINE pack #-}


----------------------------------------------------------------------------------------- DoubleAsc
-- | Human-readable ASCII Double.
data DoubleAsc  = DoubleAsc     deriving (Eq, Show)
instance Format DoubleAsc where
 type Value DoubleAsc   = Double
 fieldCount _           = 1
 minSize    _           = 1
 fixedSize  _           = Nothing

 -- Max length of a pretty-printed 64-bit double is 64 bytes.
 packedSize _ _         = Just 24
 {-# INLINE minSize    #-}
 {-# INLINE fieldCount #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable DoubleAsc where

 pack   buf DoubleAsc v k
  = do  (fptr, len)  <- storeDoubleShortest v
        F.withForeignPtr fptr $ \ptr
         -> F.copyBytes buf ptr len
        k len
 {-# INLINE pack   #-}

 unpack buf len DoubleAsc k
  = do  (v, o)       <- loadDouble buf len
        k (v, o)
 {-# INLINE unpack #-}

