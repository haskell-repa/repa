
-- | Compacting operations on arrays.
module Data.Repa.Array.Internals.Operator.Compact
        ( compact
        , compactIn )
where
import Data.Repa.Array.Internals.Layout         as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Eval.Stream                    as A
import Data.Repa.Fusion.Unpack                  as A
import Data.Repa.Stream                         as S
#include "repa-array.h"


-- | Combination of `fold` and `filter`. 
--   
--   We walk over the stream front to back, maintaining an accumulator.
--   At each point we can chose to emit an element (or not)
compact :: ( BulkI lSrc a, TargetI lDst b
           , Unpack (Buffer lDst b) t0)
        => Name lDst
        -> (s -> a -> (Maybe b, s))
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
compactIn
        :: ( BulkI lSrc a, TargetI lDst a
           , Unpack (Buffer lDst a) t0)
        => Name lDst
        -> (a -> a -> (Maybe a, a))
        -> Array lSrc a
        -> Array lDst a

compactIn nDst f arr
        = A.unstreamToArray nDst
        $ S.compactInS f 
        $ A.streamOfArray arr
{-# INLINE_ARRAY compactIn #-}


