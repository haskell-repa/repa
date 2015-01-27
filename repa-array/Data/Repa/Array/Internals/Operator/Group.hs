
module Data.Repa.Array.Internals.Operator.Group
        (groupsBy, GroupsDict)
where
import Data.Repa.Array.Shape                    as A
import Data.Repa.Array.Tuple                    as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Eval.Chain                     as A
import qualified Data.Repa.Chain                as C
#include "repa-stream.h"


-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
-- 
-- @
--  groupsBy (==) (Just ('a', 4)) 
--                [\'a\', \'a\', \'a\', \'b\', \'b\', \'c\', \'d\', \'d\'] 
--   => ([('a', 7), ('b', 2), ('c', 1)], Just (\'d\', 2))
-- @
--
groupsBy :: GroupsDict rElt rGrp rLen tGrp tLen n
         => (n -> n -> Bool)    -- ^ Comparison function.
         -> Maybe   (n, Int)    -- ^ Starting element and count.
         -> Vector  rElt n      -- ^ Input elements.
         -> (Vector (T2 rGrp rLen) (n, Int), Maybe (n, Int))

groupsBy f !c !vec0
 = (vec1, snd k1)
 where  
        f' x y = return $ f x y
        {-# INLINE f' #-}

        (vec1, k1)
         = A.unchainToVector $ C.liftChain
         $ C.groupsByC f' c  $ A.chainOfVector vec0
{-# INLINE_ARRAY groupsBy #-}


-- | Dictionaries need to perform a grouping.
type GroupsDict  rElt rGrp rLen tGrp tLen n
      = ( Bulk   rElt DIM1 n
        , Target rGrp n    tGrp
        , Target rLen Int  tLen )
