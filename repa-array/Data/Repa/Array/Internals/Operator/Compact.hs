
-- | Compacting operations on arrays.
module Data.Repa.Array.Internals.Operator.Compact
        ( compact
        , compactIn 

        , process)
where
import Data.Repa.Array.Internals.Operator.Concat        as A
import Data.Repa.Array.Internals.Layout                 as A
import Data.Repa.Array.Internals.Target                 as A
import Data.Repa.Array.Internals.Bulk                   as A
import Data.Repa.Eval.Stream                            as A
import Data.Repa.Fusion.Unpack                          as A
import Data.Repa.Stream                                 as S
import Prelude                                          hiding (concat)
#include "repa-array.h"


-- | Combination of `fold` and `filter`. 
--   
--   We walk over the stream front to back, maintaining an accumulator.
--   At each point we can chose to emit an element (or not).
--
compact :: ( BulkI lSrc a, TargetI lDst b
           , Unpack (Buffer lDst b) t0)
        => Name lDst
        -> (s -> a -> (s, Maybe b))
        -> s
        -> Array lSrc a
        -> Array lDst b

compact nDst f s0 arr
        = A.unstreamToArray nDst
        $ S.compactS f s0
        $ A.streamOfArray arr
{-# INLINE_ARRAY compact #-}


-- | Like `compact` but use the first value of the stream as the 
--   initial state, and add the final state to the end of the output.
--
compactIn
        :: ( BulkI lSrc a, TargetI lDst a
           , Unpack (Buffer lDst a) t0)
        => Name lDst
        -> (a -> a -> (a, Maybe a))
        -> Array lSrc a
        -> Array lDst a

compactIn nDst f arr
        = A.unstreamToArray nDst
        $ S.compactInS f 
        $ A.streamOfArray arr
{-# INLINE_ARRAY compactIn #-}



-- | Apply a generic stream process to an array.
--
process :: ( BulkI   lSrc a
           , TargetI lDst b, Target  lDst (Array lOut b)
           , TargetI lOut b, Bulk lOut b
           , Bulk lDst (Array lOut b)
           , Unpack (Array lOut b) t1
           , Unpack (Buffer lDst (Array lOut b)) t0)
        => Name lDst
        -> (s -> a -> (s, Array lOut b))
        -> s
        -> Array lSrc a
        -> Array lDst b

process nDst f s0 arr
 = concat  nDst 
 $ compact nDst work_process s0 arr
 where  
        work_process s x
         = case f s x of
                (s', arr') -> (s', Just arr')
        {-# INLINE_ARRAY work_process #-}
{-# INLINE process #-}

