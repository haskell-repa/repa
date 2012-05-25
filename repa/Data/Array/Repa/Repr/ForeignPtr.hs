
module Data.Array.Repa.Repr.ForeignPtr
        ( F, Array (..)
        , fromForeignPtr, toForeignPtr
        , computeIntoS,   computeIntoP)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Eval.Load
import Data.Array.Repa.Eval.Target
import Data.Array.Repa.Repr.Delayed
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import qualified Foreign.ForeignPtr.Unsafe      as Unsafe

-- | Arrays represented as foreign buffers in the C heap.
data F

-- | Read elements from a foreign buffer.
instance Storable a => Source F a where
 data Array F sh a
        = AForeignPtr !sh !Int !(ForeignPtr a)

 linearIndex (AForeignPtr _ len fptr) ix
  | ix < len  
        = unsafePerformIO 
        $ withForeignPtr fptr
        $ \ptr -> peekElemOff ptr ix
  
  | otherwise
  = error "Repa: foreign array index out of bounds"
 {-# INLINE linearIndex #-}
 
 unsafeLinearIndex (AForeignPtr _ _ fptr) ix
        = unsafePerformIO
        $ withForeignPtr fptr 
        $ \ptr -> peekElemOff ptr ix
 {-# INLINE unsafeLinearIndex #-}

 extent (AForeignPtr sh _ _)
        = sh
 {-# INLINE extent #-}

 deepSeqArray (AForeignPtr sh len fptr) x 
  = sh `deepSeq` len `seq` fptr `seq` x
 {-# INLINE deepSeqArray #-}
 

-- Load -----------------------------------------------------------------------
-- | Filling foreign buffers.
instance Storable e => Target F e where
 data MVec F e 
  = FPVec !Int !(ForeignPtr e)

 newMVec n
  = do  let (proxy :: e) = undefined
        ptr              <- mallocBytes (sizeOf proxy * n)
        _                <- peek ptr  `asTypeOf` return proxy
        
        fptr             <- newForeignPtr finalizerFree ptr
        return           $ FPVec n fptr
 {-# INLINE newMVec #-}

 -- CAREFUL: Unwrapping the foreignPtr like this means we need to be careful
 -- to touch it after the last use, otherwise the finaliser might run too early.
 unsafeWriteMVec (FPVec _ fptr) !ix !x
  = pokeElemOff (Unsafe.unsafeForeignPtrToPtr fptr) ix x
 {-# INLINE unsafeWriteMVec #-}

 unsafeFreezeMVec !sh (FPVec len fptr)
  =     return  $ AForeignPtr sh len fptr
 {-# INLINE unsafeFreezeMVec #-}

 deepSeqMVec !(FPVec _ fptr) x
  = Unsafe.unsafeForeignPtrToPtr fptr `seq` x
 {-# INLINE deepSeqMVec #-}

 touchMVec (FPVec _ fptr)
  = touchForeignPtr fptr
 {-# INLINE touchMVec #-}


-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap a `ForeignPtr` as an array.
fromForeignPtr
        :: Shape sh
        => sh -> ForeignPtr e -> Array F sh e
fromForeignPtr !sh !fptr
        = AForeignPtr sh (size sh) fptr
{-# INLINE fromForeignPtr #-}


-- | O(1). Unpack a `ForeignPtr` from an array.
toForeignPtr :: Array F sh e -> ForeignPtr e
toForeignPtr (AForeignPtr _ _ fptr)
        = fptr
{-# INLINE toForeignPtr #-}


-- | Compute an array sequentially and write the elements into a foreign
--   buffer without intermediate copying. If you want to copy a
--   pre-existing manifest array to a foreign buffer then `delay` it first.
computeIntoS
        :: (Load r1 sh e, Storable e)
        => ForeignPtr e -> Array r1 sh e -> IO ()
computeIntoS !fptr !arr
 = loadS arr (FPVec 0 fptr)
{-# INLINE computeIntoS #-}


-- | Compute an array in parallel and write the elements into a foreign
--   buffer without intermediate copying. If you want to copy a
--   pre-existing manifest array to a foreign buffer then `delay` it first.
computeIntoP
        :: (Load r1 sh e, Storable e)
        => ForeignPtr e -> Array r1 sh e -> IO ()
computeIntoP !fptr !arr
 = loadP arr (FPVec 0 fptr)
{-# INLINE computeIntoP #-}

