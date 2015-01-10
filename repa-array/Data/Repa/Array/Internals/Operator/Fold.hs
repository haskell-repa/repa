
module Data.Repa.Array.Internals.Operator.Fold
        (folds, C.Folds(..))
where
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Index          as A
import Data.Repa.Eval.Chain                     as A
import qualified Data.Repa.Chain                as C
import System.IO.Unsafe

-- | Segmented fold over vectors of segment lengths and input values.
--
--   The total lengths of all segments need not match the length of the
--   input elements vector. The returned `C.Folds` state can be inspected
--   to determine whether all segments were completely folded, or the 
--   vector of segment lengths or elements was too short relative to the
--   other.
--
folds   :: (Bulk r1 DIM1 Int, Bulk r2 DIM1 a, Target r3 b)
        => (a -> b -> b)        -- ^ Worker function.
        -> b                    -- ^ Initial state when folding segments.
        -> Maybe (Int, b)       -- ^ Length and initial state for first segment.
        ->  Vector r1 Int       -- ^ Segment lengths.
        ->  Vector r2 a         -- ^ Elements.
        -> (Vector r3 b, C.Folds Int Int a b)

folds f z s0 vLens vVals
 = unsafePerformIO
 $ do   
        (vResults, state)
                <- A.unchainToVectorIO
                $  C.foldsC (\x y -> return $ f x y) 
                         z s0
                         (A.chainOfVector vLens)
                         (A.chainOfVector vVals)

        return (vResults, C.packFolds state)
{-# INLINE [2] folds #-}
