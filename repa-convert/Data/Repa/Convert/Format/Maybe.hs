
-- | Maybe -like formats.
module Data.Repa.Convert.Format.Maybe
        ( MaybeAsc      (..))
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Repa.Convert.Internal.Packer
import Data.Repa.Convert.Internal.Unpacker
import Data.Repa.Convert.Format.Lists
import Data.Repa.Convert.Format.Binary
import Data.Word
import Data.IORef
import GHC.Exts
import Prelude hiding (fail)
#include "repa-convert.h"


------------------------------------------------------------------------------------------ MaybeAsc
-- | Maybe an Ascii string or something else.
data MaybeAsc f = MaybeAsc String f
instance Format f => Format (MaybeAsc f) where
 type Value (MaybeAsc f)   
        = Maybe (Value f)

 fieldCount _
        = 1
 {-# INLINE fieldCount #-}

 minSize    (MaybeAsc s f) 
  = let !(I# ms)   = minSize f
    in  I# (minSize_MaybeAsc s ms)
 {-# INLINE minSize    #-}

 fixedSize  (MaybeAsc s f)
  = fixedSize_MaybeAsc s (fixedSize f) 
 {-# INLINE fixedSize #-}

 packedSize (MaybeAsc str f) mv
  = case mv of
        Nothing -> Just $ length str
        Just v  -> packedSize f v
 {-# NOINLINE packedSize #-}


minSize_MaybeAsc :: String -> Int# -> Int#
minSize_MaybeAsc s i
 = case min (length s) (I# i) of
        I# i' -> i'
{-# NOINLINE minSize_MaybeAsc #-}


fixedSize_MaybeAsc :: String -> Maybe Int -> Maybe Int
fixedSize_MaybeAsc s r
 = case r of
         Nothing -> Nothing
         Just sf -> if length s == sf 
                        then Just sf
                        else Nothing
{-# NOINLINE fixedSize_MaybeAsc #-}



instance Packable f
      => Packable (MaybeAsc f) where
 pack  (MaybeAsc str f) mv
  = case mv of
        Nothing -> pack VarAsc str
        Just v  -> pack f      v
 {-# NOINLINE pack #-}

 unpack (MaybeAsc str f)
  =  Unpacker $ \start end stop fail eat
  -> do (Ptr ptr, str') <- unpackAsc (pw8 start) (pw8 end) stop

        ref             <- newIORef (error "repa-convert.unpack: undefined")
        let unpack_MaybeAsc
             = if str == str'
                then writeIORef ref (Ptr ptr, Nothing)
                else (fromUnpacker $ unpack f) start end stop fail 
                       $ \ptr' x -> writeIORef ref (Ptr ptr', Just x)
            {-# NOINLINE unpack_MaybeAsc #-}
        unpack_MaybeAsc

        (Ptr ptr', mx)  <- readIORef ref
        eat ptr' mx
 {-# INLINE unpack #-}


pw8 :: Addr# -> Ptr Word8
pw8 addr = Ptr addr
{-# INLINE pw8 #-}
