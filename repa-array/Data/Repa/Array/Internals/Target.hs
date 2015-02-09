
module Data.Repa.Array.Internals.Target
        ( Target (..),  IOBuffer, TargetI
        , fromList,     fromListInto)
where
import Data.Repa.Array.Index            as A
import Data.Repa.Array.Internals.Bulk   as A
import System.IO.Unsafe
import Control.Monad
import Control.Monad.Primitive
import Prelude                          as P


-- Target ---------------------------------------------------------------------
-- | Class of manifest array representations that can be constructed
--   in a random-access manner.
--
---
--   TODO: generalise to work with higher ranked indices.
class Layout l => Target l a where

 -- | Mutable buffer for some array representation.
 data Buffer s l a

 -- | Allocate a new mutable buffer for the given layout.
 --
 --   UNSAFE: The integer must be positive, but this is not checked.
 unsafeNewBuffer    :: PrimMonad m => l -> m (Buffer (PrimState m) l a)

 -- | Read an element from the mutable buffer.
 --
 --   UNSAFE: The index bounds are not checked.
 unsafeReadBuffer  :: PrimMonad m => Buffer (PrimState m) l a -> Int -> m a

 -- | Write an element into the mutable buffer.
 --
 --   UNSAFE: The index bounds are not checked.
 unsafeWriteBuffer  :: PrimMonad m => Buffer (PrimState m) l a -> Int -> a -> m ()

 -- | O(n). Copy the contents of a buffer that is larger by the given
 --   number of elements.
 --
 --   UNSAFE: The integer must be positive, but this is not checked.
 unsafeGrowBuffer   :: PrimMonad m => Buffer (PrimState m) l a -> Int
                                   -> m (Buffer (PrimState m) l a)

 -- | O(1). Yield a slice of the buffer without copying.
 --
 --   UNSAFE: The given starting position and length must be within the bounds
 --   of the of the source buffer, but this is not checked.
 unsafeSliceBuffer  :: PrimMonad m => Int -> Int -> Buffer (PrimState m) l a
                                   -> m (Buffer (PrimState m) l a)

 -- | O(1). Freeze a mutable buffer into an immutable Repa array.
 --
 --   UNSAFE: If the buffer is mutated further then the result of reading from
 --           the returned array will be non-deterministic.
 unsafeFreezeBuffer :: PrimMonad m => Buffer (PrimState m) l a -> m (Array l a)

 -- | O(1). Thaw an Array into a mutable buffer.
 --
 --   UNSAFE: The Array is no longer safe to use.
 unsafeThawBuffer   :: PrimMonad m => Array l a -> m (Buffer (PrimState m) l a)

 -- | Ensure the array is still live at this point.
 --   Sometimes needed when the mutable buffer is a ForeignPtr with a finalizer.
 touchBuffer        :: PrimMonad m => Buffer (PrimState m) l a -> m ()

 -- | O(1). Get the layout from a Buffer.
 bufferLayout       :: Buffer s l a -> l

type IOBuffer = Buffer RealWorld

-- | Constraint synonym that requires an integer index space.
type TargetI l a  = (Target l a, Index l ~ Int)


-------------------------------------------------------------------------------
-- | O(length src). Construct a linear array from a list of elements.
fromList :: TargetI l a
         => Name l -> [a] -> Array l a
fromList nDst xx
 = let  len      = P.length xx
        lDst     = create nDst len
        Just arr = fromListInto lDst xx
   in   arr
{-# NOINLINE fromList #-}


-- | O(length src). Construct an array from a list of elements,
--   and give it the provided layout.
--
--   The `length` of the provided shape must match the length of the list,
--   else `Nothing`.
--
fromListInto    :: Target l a
                => l -> [a] -> Maybe (Array l a)
fromListInto lDst xx
 = unsafePerformIO
 $ do   let !len = P.length xx
        if   len /= size (extent lDst)
         then return Nothing
         else do
                buf     <- unsafeNewBuffer    lDst
                zipWithM_ (unsafeWriteBuffer  buf) [0..] xx
                arr     <- unsafeFreezeBuffer buf
                return $ Just arr
{-# NOINLINE fromListInto #-}

