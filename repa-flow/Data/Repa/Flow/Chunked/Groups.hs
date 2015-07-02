
module Data.Repa.Flow.Chunked.Groups
        ( groupsBy_i
        , GroupsDict)
where
import Data.Repa.Flow.Chunked.Base
import Data.Repa.Flow.States
import Data.Repa.Fusion.Unpack
import Data.Repa.Array.Meta.Tuple               as A
import Data.Repa.Array.Generic.Index            as A
import Data.Repa.Array.Generic                  as A  hiding (GroupsDict)
import qualified Data.Repa.Flow.Generic         as G
#include "repa-flow.h"


-- | Dictionaries needed to perform a grouping.
type GroupsDict  i m lVal lGrp tGrp lLen tLen a
        = ( Flow i m lVal a, A.Index lVal ~ Int
          , TargetI  lGrp  a
          , TargetI  lLen Int
          , Unpack  (Buffer lGrp a)   tGrp
          , Unpack  (Buffer lLen Int) tLen)


-- Grouping -------------------------------------------------------------------
-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
--
-- @  
--   groupsBy (==) [4, 4, 4, 3, 3, 1, 1, 1, 4] 
--    => [3, 2, 3, 1]
-- @
-- 
groupsBy_i 
        :: GroupsDict i m lVal lGrp tGrp lLen tLen a
        => Name lGrp             -- ^ Layout for group names.
        -> Name lLen             -- ^ Layout for group lengths.
        -> (a -> a -> Bool)      -- ^ Whether successive elements should be grouped.
        ->    Sources i m lVal a -- ^ Source values.       
        -> m (Sources i m (T2 lGrp lLen) (a, Int))

groupsBy_i _ _ f (G.Sources n pull_chunk)
 = do   
        -- Refs to hold partial counts between chunks.
        refs    <- newRefs n Nothing

        let pull_groupsBy i eat eject
             = pull_chunk i eat_groupsBy eject_groupsBy
             where 
                   -- Process a chunk from the a source stream, 
                   -- using the current state we have for that stream.
                   eat_groupsBy chunk
                    = do state <- readRefs refs i
                         let (segs, state') = A.groupsWith name name f state chunk
                         writeRefs refs i state'
                         eat segs
                   {-# INLINE eat_groupsBy #-}

                   -- When there are no more chunks of source data we still
                   -- need to pass on the last count we have stored in the
                   -- state.
                   eject_groupsBy 
                    = do state <- readRefs refs i
                         case state of
                          Nothing         -> eject
                          Just seg
                           -> do writeRefs refs i Nothing
                                 eat (A.fromList name [seg])
                   {-# INLINE eject_groupsBy #-}
            {-# INLINE pull_groupsBy #-}

        return $ G.Sources n pull_groupsBy
{-# INLINE_FLOW groupsBy_i #-}


