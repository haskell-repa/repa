
-- | Grouping operations on arrays.
module Data.Repa.Array.Internals.Operator.Group
        ( groups
        , groupsWith
        , GroupsDict)
where
import Data.Repa.Array.Generic.Index            as A
import Data.Repa.Array.Meta.Tuple               as A
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
-- > import Data.Repa.Array.Material
-- > import Data.Repa.Nice
-- > nice $ groups U U (fromList U "waaabllle")
-- ([('w',1),('a',3),('b',1),('l',3)],Just ('e',1))
-- @
--
groups  :: (GroupsDict lElt lGrp tGrp lLen tLen n, Eq n)
        => Name  lGrp           -- ^ Layout for group names.
        -> Name  lLen           -- ^ Layout gor group lengths.
        -> Array lElt n         -- ^ Input elements.
        -> (Array (T2 lGrp lLen) (n, Int), Maybe (n, Int))

groups nGrp nLen arr
        = groupsWith nGrp nLen (==) Nothing arr
{-# INLINE groups #-}


-- | Like `groups`, but use the given function to determine whether two
--   consecutive elements should be in the same group. Also take
--   an initial starting group and count.
--
-- @
-- > import Data.Repa.Array.Material
-- > import Data.Repa.Nice
-- > nice $ groupsWith U U (==) (Just ('w', 5)) (fromList U "waaabllle")
-- ([('w',6),('a',3),('b',1),('l',3)],Just ('e',1))
-- @
--
groupsWith
        :: GroupsDict lElt lGrp tGrp lLen tLen n
        => Name lGrp           -- ^ Layout for group names.
        -> Name lLen           -- ^ Layout for group lengths.
        -> (n -> n -> Bool)    -- ^ Comparison function.
        -> Maybe  (n, Int)     -- ^ Starting element and count.
        -> Array  lElt n       -- ^ Input elements.
        -> (Array (T2 lGrp lLen) (n, Int), Maybe (n, Int))

groupsWith nGrp nLen f !c !vec0
 = (vec1, snd k1)
 where
        f' x y  = return $ f x y
        {-# INLINE f' #-}

        (vec1, k1)
         = A.unchainToArray (T2 nGrp nLen)
         $ C.liftChain
         $ C.groupsByC f' c
         $ A.chainOfArray vec0
{-# INLINE_ARRAY groupsWith #-}


-- | Dictionaries need to perform a grouping.
type GroupsDict  lElt lGrp tGrp lLen tLen n
      = ( Bulk   lElt n
        , Target lGrp n
        , Target lLen Int
        , Index  lGrp ~ Index lLen
        , Unpack (Buffer lLen Int) tLen
        , Unpack (Buffer lGrp n)   tGrp)


