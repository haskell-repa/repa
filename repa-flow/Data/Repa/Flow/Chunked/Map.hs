
module Data.Repa.Flow.Chunked.Map
        ( map_i,        map_o)
where
import Data.Repa.Flow.Chunked.Base
import Data.Repa.Array                          as A
import Data.Repa.Eval.Array                     as A
import qualified Data.Repa.Flow.Generic         as G
#include "repa-flow.h"


-- | Map a function over elements pulled from a source.
map_i   :: (Flow i m l1 a, A.TargetI l2 b)
        => (a -> b) -> Sources i m l1 a -> m (Sources i m l2 b)
map_i f s0 = G.smap_i (\_ c -> A.computeS name $ A.map f c) s0
{-# INLINE map_i #-}


-- | Map a function over elements pushed into a sink.
map_o   :: (Flow i m l1 a, A.TargetI l2 b)
        => (a -> b) -> Sinks i m l2 b -> m (Sinks i m l1 a)
map_o f s0 = G.smap_o (\_ c -> A.computeS name $ A.map f c) s0
{-# INLINE map_o #-}


