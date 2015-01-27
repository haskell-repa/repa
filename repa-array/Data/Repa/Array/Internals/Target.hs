
module Data.Repa.Array.Internals.Target
        ( Target (..)
        , fromList
        , vfromList)
where
import Data.Repa.Array.Index            as A
import Data.Repa.Array.Internals.Flat   as A
import Data.Repa.Array.Internals.Bulk   as A
import System.IO.Unsafe
import Control.Monad
import Prelude                          as P


-- Target ---------------------------------------------------------------------
-- | Class of manifest array representations that can be constructed 
--   in a random-access manner.
class Target r a where

 -- | Mutable buffer for some array representation.
 data Buffer r a

 -- | Allocate a new mutable buffer of the given size.
 --
 --   UNSAFE: The integer must be positive, but this is not checked.
 unsafeNewBuffer    :: Int -> IO (Buffer r a)

 -- | Write an element into the mutable buffer.
 -- 
 --   UNSAFE: The index bounds are not checked.
 unsafeWriteBuffer  :: Buffer r a -> Int -> a -> IO ()

 -- | O(n). Copy the contents of a buffer that is larger by the given
 --   number of elements.
 unsafeGrowBuffer   :: Buffer r a -> Int -> IO (Buffer r a)
 
 -- | O(1). Yield a slice of the buffer without copying.
 --
 --   UNSAFE: The given starting position and length must be within the bounds
 --   of the of the source buffer, but this is not checked.
 unsafeSliceBuffer  :: Int -> Int -> Buffer r a -> IO (Buffer r a)

 -- | O(1). Freeze a mutable buffer into an immutable Repa array.
 --
 --   UNSAFE: If the buffer is mutated further then the result of reading from
 --           the returned array will be non-deterministic.
 unsafeFreezeBuffer :: r -> ex -> sh -> Buffer r a -> IO (Array (Flat r ex sh) a)

 -- | Ensure the array is still live at this point.
 --   Sometimes needed when the mutable buffer is a ForeignPtr with a finalizer.
 touchBuffer        :: Buffer r a -> IO ()


-------------------------------------------------------------------------------
-- | O(length src). Construct an array from a list of elements,
--   and give it the provided shape. The `size` of the provided shape must
--   match the length of the list, else `Nothing`.
fromList  :: (Target r a, Shape sh)
          => r -> sh -> [a] -> Maybe (Array (Flat r Safe sh) a)
fromList r sh xx
 = unsafePerformIO
 $ do   let !len = P.length xx
        if   len /= size sh
         then return Nothing
         else do
                mvec    <- unsafeNewBuffer len
                zipWithM_ (unsafeWriteBuffer mvec) [0..] xx
                arr     <- unsafeFreezeBuffer r Safe sh mvec
                return $ Just arr
{-# NOINLINE fromList #-}


-- | O(length src). Construct a vector from a list.
vfromList :: Target r a => r -> [a] -> Vector r a
vfromList r xx
 = let  !len     = P.length xx 
        Just arr = fromList r (Z :. len) xx
   in   arr
{-# NOINLINE vfromList #-}

