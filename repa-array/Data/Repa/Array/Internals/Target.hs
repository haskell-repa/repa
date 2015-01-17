
module Data.Repa.Array.Internals.Target
        ( Target        (..)
        , fromList,     fromList_
        , vfromList,    vfromList_)
where
import Data.Repa.Array.Shape            as R
import Data.Repa.Array.Internals.Bulk   as R
import Data.Repa.Fusion.Unpack
import System.IO.Unsafe
import Control.Monad
import Prelude                          as P


-- Target ---------------------------------------------------------------------
-- | Class of manifest array representations that can be constructed 
--   in a random-access manner.
class Unpack (Buffer r e) t => Target r e t where

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

 -- | O(n). Copy the contents of a buffer that is larger by the given
 --   number of elements.
 unsafeGrowBuffer   :: Buffer r e -> Int -> IO (Buffer r e)
 
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


-------------------------------------------------------------------------------
-- | O(length src). Construct an array from a list of elements,
--   and give it the provided shape. The `size` of the provided shape must
--   match the length of the list, else `Nothing`.
fromList  :: (Shape sh, Target r a t)
          => r -> sh -> [a] -> Maybe (Array r sh a)
fromList _ sh xx = fromList_ sh xx
{-# INLINE fromList #-}


-- | Like `fromList`, but the result respresentation is implicit.
fromList_ :: (Shape sh, Target r a t)
          => sh -> [a] -> Maybe (Array r sh a)
fromList_ sh xx
 = unsafePerformIO
 $ do   let !len = P.length xx
        if   len /= size sh
         then return Nothing
         else do
                mvec    <- unsafeNewBuffer len
                zipWithM_ (unsafeWriteBuffer mvec) [0..] xx
                arr     <- unsafeFreezeBuffer sh mvec
                return $ Just arr
{-# NOINLINE fromList_ #-}


-- | O(length src). Construct a vector from a list.
vfromList :: Target r a t 
        => r -> [a] -> Vector r a
vfromList _ xx = vfromList_ xx
{-# INLINE vfromList #-}


-- | Like `vfromList`, but the result representation is implicit.
vfromList_ :: Target r a t => [a] -> Vector r a
vfromList_ xx
 = let  !len     = P.length xx 
        Just arr = fromList_ (Z :. len) xx
   in   arr
{-# NOINLINE vfromList_ #-}

