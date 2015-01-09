
module Data.Repa.Array.Internals.Operator.Fold
        (folds)
where
import Data.Repa.Array.Internals.Bulk           as R
import Data.Repa.Array.Internals.Target         as R
import Data.Repa.Array.Internals.Index          as R
import Data.Repa.Eval.Chain                     as R
import qualified Data.Repa.Chain                as C


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
        -> b                    -- ^ Initial state for each segment.
        ->  Vector r1 Int       -- ^ Segment lengths.
        ->  Vector r2 a         -- ^ Elements.
        -> (Vector r3 b, C.Folds Int Int a b)

folds f z vLens vVals
 = (vResults, C.packFolds state)
 where
        f' x y = return $ f x y
        {-# INLINE f' #-}

        (vResults, state) 
          =  R.unchainToVector $ C.liftChain
          $  C.foldsC f' z 
                (R.chainOfVector vLens)
                (R.chainOfVector vVals)
{-# INLINE [2] folds #-}

