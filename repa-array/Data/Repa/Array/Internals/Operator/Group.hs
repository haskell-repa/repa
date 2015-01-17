
module Data.Repa.Array.Internals.Operator.Group
        (groupsBy)
where
import Data.Repa.Array.Shape                    as R
import Data.Repa.Array.Internals.Bulk           as R
import Data.Repa.Array.Internals.Target         as R
import Data.Repa.Eval.Chain                     as R
import qualified Data.Repa.Chain                as C


-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
-- 
-- @
--  groupsBy (==) (Just ('a', 4)) 
--                [\'a\', \'a\', \'a\', \'b\', \'b\', \'c\', \'d\', \'d\'] 
--   => ([('a', 7), ('b', 2), ('c', 1)], Just (\'d\', 2))
-- @
--
groupsBy :: (Bulk r1 DIM1 a, Target r2 (a, Int) t)
         => (a -> a -> Bool)                -- ^ Comparison function.
         -> Maybe (a, Int)                  -- ^ Starting element and count.
         -> Vector  r1 a                    -- ^ Input elements.
         -> (Vector r2 (a, Int), Maybe (a, Int))

groupsBy f !c !vec0
 = (vec1, snd k1)
 where  
        f' x y = return $ f x y
        {-# INLINE f' #-}

        (vec1, k1)
         = R.unchainToVector $ C.liftChain
         $ C.groupsByC f' c  $ R.chainOfVector vec0
{-# INLINE [2] groupsBy #-}
