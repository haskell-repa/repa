
module Data.Repa.Convert.Format.Numeric
        ( IntAsc    (..)
        , IntAsc0   (..)
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

 -- TODO: This is very slow. Avoid going via lists.
 pack   buf IntAsc v k
   = pack buf VarAsc (show v) k
 {-# INLINE pack #-}

 unpack buf len IntAsc k 
  | len > 0
  = do  r       <- loadInt buf len
        case r of
          Just (n, o)     -> k (n, o)
          _               -> return Nothing

  | otherwise
  = return Nothing
 {-# INLINE unpack #-}


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

 -- TODO: This is very slow. Avoid going via lists.
 pack   buf (IntAsc0 n) v k
  = let s       = show v
        s'      = replicate (n - length s) '0' ++ s
    in  pack buf VarAsc s' k
 {-# INLINE pack #-}

 unpack buf len (IntAsc0 _) k 
  | len > 0
  = do  r       <- loadInt buf len
        case r of
          Just (n, o)     -> k (n, o)
          _               -> return Nothing

  | otherwise
  = return Nothing
 {-# INLINE unpack #-}


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
  | len > 0
  = do  (v, o)       <- loadDouble buf len
        k (v, o)

  | otherwise
  = return Nothing
 {-# INLINE unpack #-}


