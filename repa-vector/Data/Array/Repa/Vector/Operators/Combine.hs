
module Data.Array.Repa.Vector.Operators.Combine
        (combine2)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Unboxed
import Data.Array.Repa.Vector.Operators.Bulk
import qualified Data.Array.Repa.Flow.Seq       as F
import qualified Data.Vector.Unboxed            as U


-- | Combine two arrays with a flags vector.
--   TODO: make this parallel.
combine2 
        :: (U.Unbox a, F.Touch a)
        => Vector U Bool
        -> Vector U a
        -> Vector U a
        -> Vector U a

combine2 fs xs ys
        = AUnboxed 
                (extent fs)
                (F.unflow $ F.combine2 
                        (F.flow $ toUnboxed fs)
                        (F.flow $ toUnboxed xs) 
                        (F.flow $ toUnboxed ys))
