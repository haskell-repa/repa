
module Data.Repa.Flow.Chunked
        ( Sources, Sinks
        , Flow

          -- * Mapping
        , map_i,        map_o
        , mapc_i,       mapc_o)

where
import Data.Repa.Array                    as A
import Data.Repa.Eval.Array               as A
import qualified Data.Repa.Flow.Generic   as G


-- | A bundle of sources, where the elements are chunked into arrays.
type Sources i m r e
        = G.Sources i m (A.Vector r e)

-- | A bundle of sinks,   where the elements are chunked into arrays.
type Sinks   i m r e
        = G.Sinks   i m (A.Vector r e)


-- | Shorthand for common type classes.
class (Ord i, Monad m, Bulk r DIM1 a)
   => Flow i m r a 


-- Map ------------------------------------------------------------------------
-- | Map a function over elements pulled from a source.
map_i   :: (Flow i m r1 a, A.Target r2 b)
        => (a -> b) -> Sources i m r1 a -> m (Sources i m r2 b)
map_i f s0 = G.map_i (\_ c -> A.computeS $ A.map f c) s0
{-# INLINE [2] map_i #-}


-- | Map a function over elements pushed into a sink.
map_o   :: (Flow i m r2 a, A.Target r1 b)
        => (a -> b) -> Sinks i m r1 b -> m (Sinks i m r2 a)
map_o f s0 = G.map_o (\_ c -> A.computeS $ A.map f c) s0
{-# INLINE [2] map_o #-}


-- | Map a function over elements pulled from a source, a chunk at a time.
mapc_i  :: (Flow i m r1 a, A.Target r2 b)
        => (G.Ix i -> Vector r1 a -> Vector r2 b)
        -> Sources i m r1 a -> m (Sources i m r2 b)
mapc_i = G.map_i
{-# INLINE [2] mapc_i #-}


-- | Map a function over elements pushed to a sink, a chunk at a time.
mapc_o  :: (Flow i m r1 b, A.Target r2 b)
        => (G.Ix i -> Vector r1 a -> Vector r2 b)
        -> Sinks i m r2 b -> m (Sinks i m r1 a)
mapc_o = G.map_o
{-# INLINE [2] mapc_o #-}

