
module Data.Repa.Flow.Chunked.Base
        ( Sources, Sinks
        , Flow
        , finalize_i
        , finalize_o)
where
import Data.Repa.Flow.States
import Data.Repa.Array                    as A
import qualified Data.Repa.Flow.Generic   as G


-- | A bundle of sources, where the elements are chunked into arrays.
type Sources i m r e
        = G.Sources i m (A.Vector r e)


-- | A bundle of sinks,   where the elements are chunked into arrays.
type Sinks   i m r e
        = G.Sinks   i m (A.Vector r e)


-- | Shorthand for common type classes.
type Flow i m r a
        = (Ord i, Monad m, Bulk r DIM1 a)



-- Finalizers -----------------------------------------------------------------
-- | Attach a finalizer to a bundle of sources.
--
--   For each stream in the bundle, the finalizer will be called the first
--   time a consumer of that stream tries to pull an element when no more
--   are available.
--
--   The provided finalizer will be run after any finalizers already
--   attached to the source.
--
finalize_i
        :: States i m
        => (Ix i -> m ())
        -> Sources i m r a -> m (Sources i m r a)
finalize_i = G.finalize_i
{-# INLINE [2] finalize_i #-}


-- | Attach a finalizer to a bundle of sinks.
--
--   The finalizer will be called the first time the stream is ejected.
--
--   The provided finalizer will be run after any finalizers already
--   attached to the sink.
--
finalize_o
        :: States i m
        => (Ix i -> m ())
        -> Sinks i m r a -> m (Sinks i m r a)
finalize_o = G.finalize_o
{-# INLINE [2] finalize_o #-}
