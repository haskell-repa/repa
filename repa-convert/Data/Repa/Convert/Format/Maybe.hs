
-- | Maybe -like formats.
module Data.Repa.Convert.Format.Maybe
        ( MaybeAsc      (..))
where
import Data.Repa.Convert.Format.Lists
import Data.Repa.Convert.Format.Binary
import Data.Repa.Convert.Format.Base
import Data.Word
import GHC.Exts
import Prelude hiding (fail)
#include "repa-convert.h"


------------------------------------------------------------------------------------------ MaybeAsc
-- | Maybe an Ascii string or something else.
data MaybeAsc f = MaybeAsc String f
instance Format f => Format (MaybeAsc f) where
 type Value (MaybeAsc f)   = Maybe (Value f)

 fieldCount _              = 1
 {-# INLINE fieldCount #-}

 minSize    (MaybeAsc s f) 
  = min (length s) (minSize f)
 {-# NOINLINE minSize    #-}

 fixedSize  (MaybeAsc s f)
  = case fixedSize f of
        Nothing -> Nothing
        Just sf -> if length s == sf 
                        then Just sf
                        else Nothing
 {-# NOINLINE fixedSize #-}

 packedSize (MaybeAsc str f) mv
  = case mv of
        Nothing -> Just $ length str
        Just v  -> packedSize f v
 {-# NOINLINE packedSize #-}


instance Packable f
      => Packable (MaybeAsc f) where
 pack  (MaybeAsc str f) mv
  = case mv of
        Nothing -> pack VarAsc str
        Just v  -> pack f      v
 {-# NOINLINE pack #-}

 unpack (MaybeAsc str f)
  =  Unpacker $ \start end stop fail eat
  -> do (Ptr ptr, str')     <- unpackAsc (pw8 start) (pw8 end) stop
        if str == str'
         then eat ptr Nothing
         else (fromUnpacker $ unpack f) start end stop fail 
               $ \ptr' x -> eat ptr' (Just x)
 {-# NOINLINE unpack #-}


pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}
