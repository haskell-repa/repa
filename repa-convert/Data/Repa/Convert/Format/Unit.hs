
-- | Unitary formats.
module Data.Repa.Convert.Format.Unit
        ( UnitAsc       (..))
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Repa.Convert.Format.String
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
 packer    (UnitAsc s)              () start k
  = packer (FixChars (length s)) s  start k
 {-# INLINE pack #-}

 unpacker  (UnitAsc str) start end stop fail eat
  = do  (Ptr ptr, str') <- unpackCharList (pw8 start) (pw8 end) stop
        if str == str'
         then eat ptr ()
         else fail
 {-# NOINLINE unpack #-}



pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}
