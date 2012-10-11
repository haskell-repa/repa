
module Data.Array.Repa.Vector.Segd
        ( Segd (..)
        , empty)
where
import Data.Array.Repa          as R
import Data.Array.Repa.Vector   as R


-- | Segment descriptor.
data Segd r1 r2
        = Segd
        { -- | Length of each segment.
          lengths       :: Vector r1 Int

          -- | Starting index of each segment.
        , indices       :: Vector r2 Int

          -- | Number of elements in the flat array.
        , elements      :: Int }


-- | Construct an empty segment descriptor, 
--   with no elements or segments.
empty :: Segd D D
empty   = Segd
        { lengths       = R.fromFunction (Z :. 0) (const 0)
        , indices       = R.fromFunction (Z :. 0) (const 0)
        , elements      = 0 }


-- | Construct a segment descriptor from a vector of lengths.
-- fromLengths :: Vector r1 Int -> Segd r1 N
-- fromLengths lens
--        = Segd
--        { lengths       = lens
--        , indices       = vscanl (+) 0 lens
--        , elements      = vlength lens }

