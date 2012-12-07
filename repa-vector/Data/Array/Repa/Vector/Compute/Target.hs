
module Data.Array.Repa.Vector.Compute.Target
        ( Target    (..)
        , fromList)
where
import Data.Array.Repa.Vector.Base
import Control.Monad
import System.IO.Unsafe


-- Target ---------------------------------------------------------------------
-- | Class of manifest array representations that can be constructed in parallel.
class Target r a where

 -- | Mutable version of the representation.
 data MVec r a

 -- | Allocate a new mutable array of the given size.
 newMVec          :: Int -> IO (MVec r a)

 -- | Write an element into the mutable array.
 unsafeWriteMVec  :: MVec r a -> Int -> a -> IO ()

 -- | Freeze the mutable array into an immutable Repa array.
 --
 --   You promise not to update the source vector any further.
 unsafeFreezeMVec :: sh  -> MVec r a -> IO (Array r sh a)

 -- | Thaw a Repa array into a mutable one.
 --
 --   You promise not to read the source array again.
 unsafeThawArray  :: Array r sh a -> IO (MVec r a)

 -- | Ensure the strucure of a mutable array is fully evaluated.
 deepSeqMVec      :: MVec r a -> b -> b

 -- | Ensure the array is still live at this point.
 --   Needed when the mutable array is a `ForeignPtr` with a finalizer.
 touchMVec        :: MVec r a -> IO ()


-- | O(n). Construct a manifest array from a list.
fromList :: (Shape sh, Target r a)
         => sh -> [a] -> Array r sh a
fromList sh xx
 = unsafePerformIO
 $ do   let len = length xx
        if len /= size sh
         then error "repa-vector.fromList: provide array shape does not match list length"
         else do
                mvec    <- newMVec len
                zipWithM_ (unsafeWriteMVec mvec) [0..] xx
                unsafeFreezeMVec sh mvec
