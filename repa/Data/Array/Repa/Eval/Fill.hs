
module Data.Array.Repa.Eval.Fill
        ( Fillable  (..), fromList
        , Fill      (..)
        , FillRange (..))
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import Control.Monad
import System.IO.Unsafe

-- Fillable -------------------------------------------------------------------
-- | Class of manifest array representations that can be filled in parallel 
--   and then frozen into immutable Repa arrays.
class Fillable r e where

 -- | Mutable version of the representation.
 data MArr r e

 -- | Allocate a new mutable array of the given size.
 newMArr          :: Int -> IO (MArr r e)

 -- | Write an element into the mutable array.
 unsafeWriteMArr  :: MArr r e -> Int -> e -> IO ()

 -- | Freeze the mutable array into an immutable Repa array.
 unsafeFreezeMArr :: sh  -> MArr r e -> IO (Array r sh e)

 -- | Ensure the strucure of a mutable array is fully evaluated.
 deepSeqMArr      :: MArr r e -> a -> a

 -- | Ensure the array is still live at this point.
 --   Needed when the mutable array is a ForeignPtr with a finalizer.
 touchMArr        :: MArr r e -> IO ()


-- | O(n). Construct a manifest array from a list.
fromList
        :: (Shape sh, Fillable r e)
        => sh -> [e] -> Array r sh e
fromList sh xx
 = unsafePerformIO
 $ do   let len = length xx
        if len /= size sh
         then error "Data.Array.Repa.Eval.Fill.fromList: provide array shape does not match list length"
         else do
                marr    <- newMArr len
                zipWithM_ (unsafeWriteMArr marr) [0..] xx
                unsafeFreezeMArr sh marr


-- Fill -----------------------------------------------------------------------
-- | Compute all elements defined by an array and write them to a fillable
--   representation.
--  
--   Note that instances require that the source array to have a delayed
--   representation such as `D` or `C`. If you want to use a pre-existing
--   manifest array as the source then `delay` it first.
class (Source r1 sh e, Fillable r2 e) => Fill r1 r2 sh e where
 -- | Fill an entire array sequentially.
 fillS          :: Array r1 sh e -> MArr r2 e -> IO ()

 -- | Fill an entire array in parallel.
 fillP          :: Array r1 sh e -> MArr r2 e -> IO ()


-- FillRange ------------------------------------------------------------------
-- | Compute a range of elements defined by an array and write them to a fillable
--   representation.
class (Source r1 sh e, Fillable r2 e) => FillRange r1 r2 sh e where
 -- | Fill a range of an array sequentially.
 fillRangeS     :: Array r1 sh e -> MArr r2 e -> sh -> sh -> IO ()

 -- | Fill a range of an array in parallel.
 fillRangeP     :: Array r1 sh e -> MArr r2 e -> sh -> sh -> IO ()


                        
