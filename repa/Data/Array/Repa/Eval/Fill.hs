
module Data.Array.Repa.Eval.Fill
        ( Fillable  (..)
        , Fill      (..)
        , FillRange (..))
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape

-- | Class of manifest array representations that can be filled in parallel.
class Fillable r e where

 -- | Mutable version of the representation.
 data MArr r e

 -- | Allocate a new array of the given size.
 newMArr          :: Int -> IO (MArr r e)

 -- | Write an element into the mutable array.
 unsafeWriteMArr  :: MArr r e -> Int -> e -> IO ()

 -- | Freeze the mutable array into a Repa array.
 unsafeFreezeMArr :: sh  -> MArr r e -> IO (Array r sh e)


-- | Compute array elements and write them to a filleable
--   array representation.
--  
--   Note that instances require that the source array to have a delayed
--   representation such as `D` or `C`. If you want to use a pre-existing
--   manifest array as the source then `delay` it first.
class (Shape sh, Repr r1 e, Fillable r2 e) => Fill r1 r2 sh e where
 -- | Fill an entire array sequentially.
 fillS          :: Array r1 sh e -> MArr r2 e -> IO ()

 -- | Fill an entire array in parallel.
 fillP          :: Array r1 sh e -> MArr r2 e -> IO ()


class (Shape sh, Repr r1 e, Fillable r2 e) => FillRange r1 r2 sh e where
 -- | Fill a range of an array sequentially.
 fillRangeS     :: Array r1 sh e -> MArr r2 e -> sh -> sh -> IO ()

 -- | Fill a range of an array in parallel.
 fillRangeP     :: Array r1 sh e -> MArr r2 e -> sh -> sh -> IO ()
