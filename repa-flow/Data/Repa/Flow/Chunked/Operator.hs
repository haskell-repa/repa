
-- | Operators for chunked flows.
--
--   Most functions in this module are re-exports of the ones from
--   "Data.Repa.Flow.Generic.IO", but using the `Sources` and `Sinks`
--   type synonyms for chunked flows.
--
module Data.Repa.Flow.Chunked.Operator
        ( -- * Mapping
          map_i,        map_o
        , mapChunks_i,  mapChunks_o
        , smapChunks_i, smapChunks_o

          -- * Watching
        , watch_i,      watch_o
        , trigger_o

          -- * Ignorance
        , discard_o
        , ignore_o)
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


-- Watch ----------------------------------------------------------------------
-- | Apply a monadic function to every chunk pulled from some sources,
--   producing some new sources.
watch_i :: Flow i m r a
        => (G.Ix i -> Vector r a -> m ()) 
        -> Sources i m r a  -> m (Sources i m r a)
watch_i = G.watch_i
{-# INLINE [2] watch_i #-}


-- | Pass chunks to the provided action as they are pushed into the sink.
watch_o :: Flow i m r a
        => (G.Ix i -> Vector r a -> m ())
        -> Sinks i m r a ->  m (Sinks i m r a)

watch_o = G.watch_o
{-# INLINE [2] watch_o #-}


-- | Like `watch` but doesn't pass elements to another sink.
trigger_o :: Flow i m r a
          => i -> (G.Ix i -> Vector r a -> m ()) -> m (Sinks i m r a)
trigger_o = G.trigger_o
{-# INLINE [2] trigger_o #-}


-- Grouping -------------------------------------------------------------------
-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
-- 
--   Example: groupBy (==) [4, 4, 4, 3, 3, 1, 1, 1, 4] = [3, 2, 3, 1]
--
{-
groupBy_i 
        :: Flow i m r a
        => Sources i m r a -> m (Source i m r a)
groupBy_i 
 = do   refs    <- newRefs n Nothing

        let pull_groupBy i eat eject
             = do xs'   <- readRefs refs i
-}




-- Discard --------------------------------------------------------------------
-- | A sink that drops all data on the floor.
--
--   This sink is strict in the *chunks*, so they are demanded before being
--   discarded. Haskell debugging thunks attached to the chunks will be
--   demanded, but thunks attached to elements may not be -- depending on
--   whether the chunk representation is strict in the elements.
--
discard_o :: Monad m => i -> m (Sinks i m r a)
discard_o = G.discard_o
{-# INLINE [2] discard_o #-}


-- | A sink that ignores all incoming data.
--
--   This sink is non-strict in the chunks. 
--   Haskell tracing thunks attached to the chunks will *not* be demanded.
--
ignore_o :: Monad m => i -> m (Sinks i m r a)
ignore_o  = G.ignore_o
{-# INLINE [2] ignore_o #-}

