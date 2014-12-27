
module Data.Repa.Flow.Chunked.Operator
        ( map_i,        map_o
        , mapChunks_i,  mapChunks_o
        , smapChunks_i, smapChunks_o)
where
import Data.Repa.Flow.Chunked.Base
import Data.Repa.Array                    as A
import Data.Repa.Eval.Array               as A
import qualified Data.Repa.Flow.Generic   as G


-- Mapping --------------------------------------------------------------------
-- | Map a function over elements pulled from a source.
map_i   :: (Flow i m r1 a, A.Target r2 b)
        => (a -> b) -> Sources i m r1 a -> m (Sources i m r2 b)
map_i f s0 = G.smap_i (\_ c -> A.computeS $ A.map f c) s0
{-# INLINE [2] map_i #-}


-- | Map a function over elements pushed into a sink.
map_o   :: (Flow i m r1 a, A.Target r2 b)
        => (a -> b) -> Sinks i m r2 b -> m (Sinks i m r1 a)
map_o f s0 = G.smap_o (\_ c -> A.computeS $ A.map f c) s0
{-# INLINE [2] map_o #-}


-- | Map a function over elements pulled from a source, a chunk at a time.
mapChunks_i  
        :: Monad m
        => (Vector r1 a -> Vector r2 b)
        -> Sources i m r1 a -> m (Sources i m r2 b)
mapChunks_i f s = G.smap_i (\_ c -> f c) s
{-# INLINE [2] mapChunks_i #-}


-- | Map a function over elements pushed to a sink, a chunk at a time.
mapChunks_o  
        :: Monad m
        => (Vector r1 a -> Vector r2 b)
        -> Sinks i m r2 b -> m (Sinks i m r1 a)
mapChunks_o f s = G.smap_o (\_ c -> f c) s
{-# INLINE [2] mapChunks_o #-}



-- | Map a function over elements pulled from a source, a chunk at a time.
--  
--   The worker function is also given the stream index.
smapChunks_i  
        :: Monad m
        => (G.Ix i -> Vector r1 a -> Vector r2 b)
        -> Sources i m r1 a -> m (Sources i m r2 b)
smapChunks_i = G.smap_i
{-# INLINE [2] smapChunks_i #-}


-- | Map a function over elements pushed to a sink, a chunk at a time.
-- 
--   The worker function is also given the stream index.
smapChunks_o  
        :: Monad m
        => (G.Ix i -> Vector r1 a -> Vector r2 b)
        -> Sinks i m r2 b -> m (Sinks i m r1 a)
smapChunks_o = G.smap_o
{-# INLINE [2] smapChunks_o #-}
