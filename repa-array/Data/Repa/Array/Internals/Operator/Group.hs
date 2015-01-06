
module Data.Repa.Array.Internals.Operator.Group
        (groupsBy)
where
import Data.Repa.Array.Internals.Bulk           as R
import Data.Repa.Array.Internals.Target         as R
import Data.Repa.Array.Internals.Index          as R
import Data.Repa.Eval.Stream                    as R
import Data.Repa.Eval.Chain                     as R
import qualified Data.Repa.Chain                as C


-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
-- 
-- @
--  groupsBy (==) [\'a\', \'a\', \'a\', \'b\', \'b\', \'c\', \'d\', \'d\'] 
--            = ([3, 2, 1], Just (\'d\', 2))
-- @
--
groupsBy :: (Bulk r1 DIM1 a, Target r2 Int)
        => (a -> a -> Bool)                -- ^ Comparison function.
        -> (Vector r1 a,   Maybe (a, Int)) -- ^ Input values and starting state.
        -> (Vector r2 Int, Maybe (a, Int)) -- ^ Segment lengths and ending state.

groupsBy f (vec, c)
 = (vec', snd c')
 where  (vec', c')
         = R.unchainToVector 
         $ C.liftChain
         $ C.resume ((), c)
         $ C.groupsByC f
         $ C.chainOfStream ()
         $ R.stream vec
{-# INLINE [2] groupsBy #-}
