
-- | Unitary formats.
module Data.Repa.Convert.Format.Unit
        ( UnitAsc       (..))
where
import Data.Repa.Convert.Format.Lists
import Data.Repa.Convert.Format.Binary
import Data.Repa.Convert.Format.Base
import Prelude hiding (fail)


------------------------------------------------------------------------------------------- UnitAsc
-- | A particular ASCII string.
data UnitAsc    = UnitAsc String        deriving (Eq, Show)
instance Format UnitAsc                 where
 type Value UnitAsc        = ()
 fieldCount _              = 1
 minSize    (UnitAsc s)    = length s
 fixedSize  (UnitAsc s)    = Just $ length s
 packedSize (UnitAsc s) () = Just $ length s
 {-# INLINE fieldCount #-}
 {-# INLINE minSize    #-}
 {-# INLINE fixedSize  #-}
 {-# INLINE packedSize #-}


instance Packable UnitAsc where
 pack   (UnitAsc s) ()    
  = pack (FixAsc (length s)) s
 {-# INLINE_INNER pack #-}

 unpack (UnitAsc str) 
  =  Unpacker $ \start end stop fail eat
  -> do (ptr, str')     <- unpackAsc start end stop
        if str == str'
         then eat ptr ()
         else fail
 {-# INLINE_INNER unpack #-}

