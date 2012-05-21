
module Data.Array.Repa.Eval.Target
        ( Target    (..)
        , fromList)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import Control.Monad
import System.IO.Unsafe


-- Target ---------------------------------------------------------------------
-- | Class of manifest array representations that can be constructed in parallel.
class Target r e where

 -- | Mutable version of the representation.
 data MVec r e

 -- | Allocate a new mutable array of the given size.
 newMVec          :: Int -> IO (MVec r e)

 -- | Write an element into the mutable array.
 unsafeWriteMVec  :: MVec r e -> Int -> e -> IO ()

 -- | Freeze the mutable array into an immutable Repa array.
 unsafeFreezeMVec :: sh  -> MVec r e -> IO (Array r sh e)

 -- | Ensure the strucure of a mutable array is fully evaluated.
 deepSeqMVec      :: MVec r e -> a -> a

 -- | Ensure the array is still live at this point.
 --   Needed when the mutable array is a ForeignPtr with a finalizer.
 touchMVec        :: MVec r e -> IO ()


-- | O(n). Construct a manifest array from a list.
fromList :: (Shape sh, Target r e)
         => sh -> [e] -> Array r sh e
fromList sh xx
 = unsafePerformIO
 $ do   let len = length xx
        if len /= size sh
         then error "Data.Array.Repa.Eval.Fill.fromList: provide array shape does not match list length"
         else do
                mvec    <- newMVec len
                zipWithM_ (unsafeWriteMVec mvec) [0..] xx
                unsafeFreezeMVec sh mvec


