
-- | Unitary formats.
module Data.Repa.Convert.Format.Unit
        ( UnitAsc       (..))
where
import Data.Repa.Convert.Format.Lists
import Data.Repa.Convert.Format.Binary
import Data.Repa.Convert.Format.Base
import GHC.Exts
import Data.Word
import Prelude hiding (fail)
#include "repa-convert.h"


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
 {-# NOINLINE pack #-}

 unpack (UnitAsc str) 
  =  Unpacker $ \start end stop fail eat
  -> do (Ptr ptr, str')     <- unpackAsc (pw8 start) (pw8 end) stop
        if str == str'
         then eat ptr ()
         else fail
 {-# NOINLINE unpack #-}



pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}
