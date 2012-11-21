
module Data.Array.Repa.Vector.Operators.Project
        ( gather
        , gather1)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Operators.Bulk
import Data.Array.Repa.Vector.Operators.Map     as R
import Prelude                                  hiding (map)


-- | Gather elements from a shared array.
gather  :: (Shape sh, Bulk r1 a, Map r2 sh)
        => Array r1 sh a -> Vector r2 sh -> Vector (TM r2) a
gather vec ixs
        = R.map (index vec) ixs
{-# INLINE [4] gather #-}


-- | Like gather, but specialised to linear indices.
gather1 :: (Bulk r1 a, Map r2 Int)
        => Vector r1 a -> Vector r2 Int -> Vector (TM r2) a
gather1 vec ixs
        = R.map (linearIndex vec) ixs
{-# INLINE gather1 #-}
