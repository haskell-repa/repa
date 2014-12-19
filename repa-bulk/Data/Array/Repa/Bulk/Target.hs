
module Data.Array.Repa.Bulk.Target
        ( Target    (..)
        , fromList)
where
import Data.Array.Repa.Bulk.Base
import Data.Array.Repa.Shape
import System.IO.Unsafe
import Control.Monad


-- Target ---------------------------------------------------------------------
-- | Class of manifest array representations that can be constructed 
--   in a random-access manner.
class Target r e where

 -- | Mutable buffer for some array representation.
 data Buffer r e

 -- | Allocate a new mutable buffer of the given size.
 --
 --   UNSAFE: The integer must be positive, but this is not checked.
 unsafeNewBuffer    :: Int -> IO (Buffer r e)

 -- | Write an element into the mutable buffer.
 -- 
 --   UNSAFE: The index bounds are not checked.
 unsafeWriteBuffer  :: Buffer r e -> Int -> e -> IO ()

 -- | O(1). Yield a slice of the buffer without copying.
 --
 --   UNSAFE: The given starting position and length must be within the bounds
 --   of the of the source buffer, but this is not checked.
 unsafeSliceBuffer  :: Int -> Int -> Buffer r e -> IO (Buffer r e)
        -- TODO: cannot use existing load functions on a windowed buffer,
        --  because we lose the linear indexing.

 -- | O(1). Freeze a mutable buffer into an immutable Repa array.
 --
 --   UNSAFE: If the buffer is mutated further then the result of reading from
 --           the returned array will be non-deterministic.
 unsafeFreezeBuffer :: Shape sh => sh  -> Buffer r e -> IO (Array r sh e)

 -- | Ensure the array is still live at this point.
 --   Sometimes needed when the mutable buffer is a ForeignPtr with a finalizer.
 touchBuffer        :: Buffer r e -> IO ()


-- | O(n). Construct an array from a list.
--   The `size` of the given shape must match the length of the list,
--   else `Nothing`.
fromList  :: (Shape sh, Target r e)
        => sh -> [e] -> Maybe (Array r sh e)
fromList sh xx
 = unsafePerformIO
 $ do   let !len = length xx
        if   len /= size sh
         then return Nothing
         else do
                mvec    <- unsafeNewBuffer len
                zipWithM_ (unsafeWriteBuffer mvec) [0..] xx
                arr     <- unsafeFreezeBuffer sh mvec
                return $ Just arr
{-# INLINE [1] fromList #-}
