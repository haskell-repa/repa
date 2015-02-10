
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
        , ignore_o

          -- * Grouping
        , groupsBy_i,   F.GroupsDict

          -- * Folding
        , folds_i,      F.FoldsDict)
where
import Data.Repa.Flow.Chunked.Operator.Groups   as F
import Data.Repa.Flow.Chunked.Operator.Folds    as F
import Data.Repa.Flow.Chunked.Base
import Data.Repa.Array                          as A
import Data.Repa.Eval.Array                     as A
import qualified Data.Repa.Flow.Generic         as G
#include "repa-stream.h"


-- Mapping --------------------------------------------------------------------
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


-- | Map a function over elements pulled from a source, a chunk at a time.
mapChunks_i  
        :: Monad m
        => (Array l1 a -> Array l2 b)
        -> Sources i m l1 a -> m (Sources i m l2 b)
mapChunks_i f s = G.smap_i (\_ c -> f c) s
{-# INLINE mapChunks_i #-}


-- | Map a function over elements pushed to a sink, a chunk at a time.
mapChunks_o  
        :: Monad m
        => (Array l1 a -> Array l2 b)
        -> Sinks i m l2 b -> m (Sinks i m l1 a)
mapChunks_o f s = G.smap_o (\_ c -> f c) s
{-# INLINE mapChunks_o #-}


-- | Like `mapChunks_i`, except that the worker function is also given
--   the source index.
smapChunks_i  
        :: Monad m
        => (i -> Array l1 a -> Array l2 b)
        -> Sources i m l1 a -> m (Sources i m l2 b)
smapChunks_i = G.smap_i
{-# INLINE smapChunks_i #-}


-- | Like `mapChunks_o`, except that the worker function is also given
--   the sink index.
smapChunks_o  
        :: Monad m
        => (i -> Array l1 a -> Array l2 b)
        -> Sinks i m l2 b -> m (Sinks i m l1 a)
smapChunks_o = G.smap_o
{-# INLINE smapChunks_o #-}


-- Watch ----------------------------------------------------------------------
-- | Hook a monadic function to some sources, which will be passed every
--   chunk that is pulled from the result.
watch_i :: Monad m
        => (i -> Array l a -> m ()) 
        -> Sources i m l a  -> m (Sources i m l a)
watch_i = G.watch_i
{-# INLINE watch_i #-}


-- | Hook a monadic function to some sinks, which will be passed every 
--   chunk that is pushed to the result.
watch_o :: Monad m
        => (i -> Array l a -> m ())
        -> Sinks i m l a ->  m (Sinks i m l a)

watch_o = G.watch_o
{-# INLINE watch_o #-}


-- | Like `watch_o` but discard the incoming chunks after they are passed
--   to the function.
trigger_o :: Monad m
          => i -> (i -> Array l a -> m ()) -> m (Sinks i m l a)
trigger_o = G.trigger_o
{-# INLINE trigger_o #-}


-- Ignorance ------------------------------------------------------------------
-- | A sink that ignores all incoming data.
--
--   This sink is non-strict in the chunks. 
--   Haskell tracing thunks attached to the chunks will *not* be demanded.
--
ignore_o :: Monad m => i -> m (Sinks i m l a)
ignore_o  = G.ignore_o
{-# INLINE ignore_o #-}


-- | Yield a bundle of sinks of the given arity that drops all data on the
--   floor.
--
--   * The sinks is strict in the *chunks*, so they are demanded before being
--     discarded. Haskell debugging thunks attached to the chunks will be
--     demanded, but thunks attached to elements may not be -- depending on
--     whether the chunk representation is strict in the elements.
--
discard_o :: Monad m => i -> m (Sinks i m l a)
discard_o = G.discard_o
{-# INLINE discard_o #-}

