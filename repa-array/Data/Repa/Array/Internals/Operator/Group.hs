
module Data.Repa.Array.Internals.Operator.Group
        (groupsBy, GroupsDict)
where
import Data.Repa.Array.Index                    as A
import Data.Repa.Array.Tuple                    as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Fusion.Unpack                  as A
import Data.Repa.Eval.Chain                     as A
import qualified Data.Repa.Chain                as C
#include "repa-array.h"


-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
-- 
-- @
--  groupsBy (==) (Just ('a', 4)) 
--                [\'a\', \'a\', \'a\', \'b\', \'b\', \'c\', \'d\', \'d\'] 
--   => ([('a', 7), ('b', 2), ('c', 1)], Just (\'d\', 2))
-- @
--
groupsBy :: GroupsDict lElt lGrp tGrp lLen tLen n
         => lGrp                -- ^ Layout for group names.
         -> lLen                -- ^ Layour for group lengths.
         -> (n -> n -> Bool)    -- ^ Comparison function.
         -> Maybe  (n, Int)     -- ^ Starting element and count.
         -> Array  lElt n       -- ^ Input elements.
         -> (Array (T2 lGrp lLen) (n, Int), Maybe (n, Int))

groupsBy lGrp lLen f !c !vec0
 = (vec1, snd k1)
 where  
        f' x y = return $ f x y
        {-# INLINE f' #-}

        (vec1, k1)
         = A.unchainToArray (T2 lGrp lLen) 
         $ C.liftChain
         $ C.groupsByC f' c  
         $ A.chainOfArray vec0
{-# INLINE_ARRAY groupsBy #-}


-- | Dictionaries need to perform a grouping.
type GroupsDict  lElt lGrp tGrp lLen tLen n
      = ( Bulk   lElt n
        , Target lGrp n
        , Target lLen Int 
        , Index  lGrp ~ Index lLen
        , Unpack (Buffer lLen Int) tLen
        , Unpack (Buffer lGrp n)   tGrp)

      
