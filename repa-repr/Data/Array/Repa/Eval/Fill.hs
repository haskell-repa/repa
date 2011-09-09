
module Data.Array.Repa.Eval.Fill
        (Fill (..))
where
import Data.Array.Repa.Base
import Data.Array.Repa.Eval.Elt


-- | Class of array representations that can be filled in parallel.
class Elt e => Fill r e where

 -- | Mutable version of the representation.
 data MArr r e

 -- | Allocate a new array of the given size.
 newMArr      :: Int -> IO (MArr r e)

 -- | Write an element into the mutable array.
 writeMArr    :: MArr r e -> Int -> e -> IO ()

 -- | Freeze the mutable array into a Repa array.
 unsafeFreezeMArr :: sh  -> MArr r e -> IO (Array r sh e)
