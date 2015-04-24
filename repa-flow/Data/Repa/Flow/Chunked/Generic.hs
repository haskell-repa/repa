
-- | Operators for chunked flows.
--
--   Functions in this module are re-exports of the ones from
--   "Data.Repa.Flow.Generic", but using the `Sources` and `Sinks`
--   type synonyms for chunked flows.
--
module Data.Repa.Flow.Chunked.Generic
        ( -- * Watching
          watch_i,      watch_o
        , trigger_o

          -- * Ignorance
        , ignore_o
        , abandon_o)
where
import Data.Repa.Flow.Chunked.Base
import Data.Repa.Array.Generic                  as A
import qualified Data.Repa.Flow.Generic         as G
#include "repa-flow.h"


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
--   * The sinks is strict in the *chunks*, so they are demanded before being
--     discarded. Haskell debugging thunks attached to the chunks will be
--     demanded, but thunks attached to elements may not be -- depending on
--     whether the chunk representation is strict in the elements.
--
ignore_o :: Monad m => i -> m (Sinks i m l a)
ignore_o  = G.ignore_o
{-# INLINE ignore_o #-}


-- | Yield a bundle of sinks of the given arity that drops all data on the
--   floor.
--
--   This sink is non-strict in the chunks. 
--   Haskell tracing thunks attached to the chunks will *not* be demanded.
--
abandon_o :: Monad m => i -> m (Sinks i m l a)
abandon_o = G.abandon_o
{-# INLINE abandon_o #-}

