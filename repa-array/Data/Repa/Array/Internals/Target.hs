
module Data.Repa.Array.Internals.Target
        ( Target (..)
        , fromList)
where
import Data.Repa.Array.Index            as A
import Data.Repa.Array.Internals.Bulk   as A
import System.IO.Unsafe
import Control.Monad
import Prelude                          as P


-- Target ---------------------------------------------------------------------
-- | Class of manifest array representations that can be constructed 
--   in a random-access manner.
--
--   TODO: generalise unsafeGrowBuffer to work with higher ranked dims.
--
class Layout l => Target l a where

 -- | Mutable buffer for some array representation.
 data Buffer l a

 -- | Allocate a new mutable buffer for the given layout.
 --
 --   UNSAFE: The integer must be positive, but this is not checked.
 unsafeNewBuffer    :: l -> IO (Buffer l a)

 -- | Write an element into the mutable buffer.
 -- 
 --   UNSAFE: The index bounds are not checked.
 unsafeWriteBuffer  :: Buffer l a -> Int -> a -> IO ()

 -- | O(n). Copy the contents of a buffer that is larger by the given
 --   number of elements.
 unsafeGrowBuffer   :: Buffer l a -> Int -> IO (Buffer l a)
 
 -- | O(1). Yield a slice of the buffer without copying.
 --
 --   UNSAFE: The given starting position and length must be within the bounds
 --   of the of the source buffer, but this is not checked.
 unsafeSliceBuffer  :: Int -> Int -> Buffer l a -> IO (Buffer l a)

 -- | O(1). Freeze a mutable buffer into an immutable Repa array.
 --
 --   UNSAFE: If the buffer is mutated further then the result of reading from
 --           the returned array will be non-deterministic.
 unsafeFreezeBuffer :: Buffer l a -> IO (Array l a)

 -- | Ensure the array is still live at this point.
 --   Sometimes needed when the mutable buffer is a ForeignPtr with a finalizer.
 touchBuffer        :: Buffer l a -> IO ()


-------------------------------------------------------------------------------
-- | O(length src). Construct an array from a list of elements,
--   and give it the provided shape. The `size` of the provided shape must
--   match the length of the list, else `Nothing`.
fromList  :: Target l a
          => l -> [a] -> Maybe (Array l a)
fromList l xx
 = unsafePerformIO
 $ do   let !len = P.length xx
        if   len /= size (extent l)
         then return Nothing
         else do
                buf     <- unsafeNewBuffer    l
                zipWithM_ (unsafeWriteBuffer  buf) [0..] xx
                arr     <- unsafeFreezeBuffer buf
                return $ Just arr
{-# NOINLINE fromList #-}

