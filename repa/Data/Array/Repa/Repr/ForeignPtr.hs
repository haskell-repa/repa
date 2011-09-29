
module Data.Array.Repa.Repr.ForeignPtr
        ( F, Array (..)
        , fromForeignPtr, toForeignPtr)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Eval.Fill
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.List
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import System.IO.Unsafe

-- | Arrays represented as foreign buffers in the C heap.
data F
data instance Array F sh e
        = AForeignPtr sh !Int !(ForeignPtr e)

-- Repr -----------------------------------------------------------------------
-- | Use elements from an unboxed vector array.
instance Storable a => Repr F a where
 {-# INLINE linearIndex #-}
 linearIndex (AForeignPtr _ len fptr) ix
  | ix < len  
        = unsafePerformIO 
        $ withForeignPtr fptr
        $ \ptr -> peekElemOff ptr ix
  
  | otherwise
  = error "Data.Array.Repa.Repr.ForeignPtr.linearIndes: array index out of bounds"

 {-# INLINE unsafeLinearIndex #-}
 unsafeLinearIndex (AForeignPtr _ _ fptr) ix
        = unsafePerformIO
        $ withForeignPtr fptr 
        $ \ptr -> peekElemOff ptr ix

 {-# INLINE extent #-}
 extent (AForeignPtr sh _ _)
        = sh

 {-# INLINE deepSeqArray #-}
 deepSeqArray (AForeignPtr sh len fptr) x 
  = sh `deepSeq` len `seq` fptr `seq` x


-- Fill -----------------------------------------------------------------------
-- | Filling of foreign buffers.
instance Storable e => Fillable F e where
 data MArr F e 
  = FPArr !Int !(ForeignPtr e)

 {-# INLINE newMArr #-}
 newMArr n
  = do  let (proxy :: e) = undefined
        ptr              <- mallocBytes (sizeOf proxy * n)
        _                <- peek ptr  `asTypeOf` return proxy
        
        fptr             <- newForeignPtr finalizerFree ptr
        return           $ FPArr n fptr

 {-# INLINE unsafeWriteMArr #-}
 unsafeWriteMArr (FPArr _ fptr) ix x
  = withForeignPtr fptr
  $ \ptr -> pokeElemOff ptr ix x

 {-# INLINE unsafeFreezeMArr #-}
 unsafeFreezeMArr sh (FPArr len fptr)     
  =     return  $ AForeignPtr sh len fptr



-- Load -----------------------------------------------------------------------
-- | no-op.
instance Shape sh => Load F F sh e where
 {-# INLINE load #-}
 load arr = arr



-- Conversions ----------------------------------------------------------------
-- | O(1). Wrap an unboxed vector as an array.
fromForeignPtr
        :: Shape sh
        => sh -> ForeignPtr e -> Array F sh e
{-# INLINE fromForeignPtr #-}
fromForeignPtr sh fptr
        = AForeignPtr sh (size sh) fptr


-- | Convert a Repa array to a `ForeignPtr` array.
-- 
--   This is O(1) if the source is already represented as a `ForeignPtr` array (has representation `F`).
--
toForeignPtr
        :: Load r1 F sh e
        => Array r1 sh e -> ForeignPtr e
{-# INLINE toForeignPtr #-}
toForeignPtr arr
 = case load arr of
        AForeignPtr _ _ fptr  -> fptr



