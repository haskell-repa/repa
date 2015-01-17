
module Data.Repa.Array.Internals.Operator.Fold
        (folds, C.Folds(..))
where
import Data.Repa.Array.Shape                    as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Eval.Chain                     as A
import qualified Data.Repa.Chain                as C
import Data.Repa.Fusion.Option
import System.IO.Unsafe


-- | Segmented fold over vectors of segment lengths and input values.
--
--   The total lengths of all segments need not match the length of the
--   input elements vector. The returned `C.Folds` state can be inspected
--   to determine whether all segments were completely folded, or the 
--   vector of segment lengths or elements was too short relative to the
--   other.
--
folds   :: (Bulk r1 DIM1 (n, Int), Bulk r2 DIM1 a, Target r3 (n, b) t)
        => (a -> b -> b)        -- ^ Worker function.
        -> b                    -- ^ Initial state when folding segments.
        -> Option3 n Int b      -- ^ Length and initial state for first segment.
        ->  Vector r1 (n, Int)  -- ^ Segment lengths.
        ->  Vector r2 a         -- ^ Elements.
        -> (Vector r3 (n, b), C.Folds Int Int n a b)

folds f z s0 vLens vVals
 = unsafePerformIO
 $ do   
        let f' !x !y = return $ f x y
            {-# INLINE f' #-}

        A.unchainToVectorIO
         $  C.foldsC f' z s0
                (A.chainOfVector vLens)
                (A.chainOfVector vVals)
{-# INLINE [3] folds #-}
