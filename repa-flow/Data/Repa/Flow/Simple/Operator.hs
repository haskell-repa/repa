
module Data.Repa.Flow.Simple.Operator
        ( -- * Constructors
          repeat_i
        , replicate_i
        , prepend_i

          -- * Mapping
        , map_i,        map_o

          -- * Connecting
        , dup_oo,       dup_io,         dup_oi
        , connect_i

          -- * Splitting
        , head_i
        , peek_i

          -- * Grouping
        , groups_i

          -- * Packing
        , pack_ii

          -- * Folding
        , folds_ii

          -- * Watching
        , watch_i
        , watch_o
        , trigger_o

          -- * Ignorance
        , ignore_o
        , abandon_o)
where
import Data.Repa.Flow.Simple.Base
import Data.Repa.Flow.States                    (States (..))
import qualified Data.Repa.Flow.Generic         as G
#include "repa-flow.h"


-- Constructors ---------------------------------------------------------------
-- | Yield a source that always produces the same value.
repeat_i :: States () m
         => a -> m (Source m a)
repeat_i x 
        = G.repeat_i () (const x)
{-# INLINE repeat_i #-}


-- | Yield a source of the given length that always produces the same value.
replicate_i 
        :: States () m
        => Int -> a -> m (Source m a)
replicate_i n x 
        = G.replicate_i () n (const x)
{-# INLINE replicate_i #-}


-- | Prepend some more elements to the front of a source.
prepend_i :: States () m
          => [a] -> Source m a -> m (Source m a)
prepend_i = G.prepend_i
{-# INLINE prepend_i #-}


-- Mapping --------------------------------------------------------------------
-- | Apply a function to every element pulled from some source, 
--   producing a new source.
map_i     :: States () m => (a -> b) -> Source m a -> m (Source m b)
map_i f s =  G.smap_i (\_ x -> f x) s
{-# INLINE map_i #-}


-- | Apply a function to every element pushed to some sink,
--   producing a new sink.
map_o     :: States () m => (a -> b) -> Sink   m b -> m (Sink   m a)
map_o f s = G.smap_o (\_ x -> f x) s
{-# INLINE map_o #-}


-- Connecting -----------------------------------------------------------------
-- | Send the same data to two consumers.
--
--   Given two argument sinks, yield a result sink.
--   Pushing to the result sink causes the same element to be pushed to both
--   argument sinks. 
dup_oo    :: States () m => Sink m a   -> Sink m a -> m (Sink m a)
dup_oo    =  G.dup_oo
{-# INLINE dup_oo #-}


-- | Send the same data to two consumers.
--  
--   Given an argument source and argument sink, yield a result source.
--   Pulling an element from the result source pulls from the argument source,
--   and pushes that element to the sink, as well as returning it via the
--   result source.
dup_io    :: States () m => Source m a -> Sink m a -> m (Source m a)
dup_io    =  G.dup_io
{-# INLINE dup_io #-}


-- | Send the same data to two consumers.
--
--   Like `dup_io` but with the arguments flipped.
--
dup_oi    :: States () m => Sink m a   -> Source m a -> m (Source m a)
dup_oi    =  G.dup_oi
{-# INLINE dup_oi #-}


-- | Connect an argument source to two result sources.
--
--   Pulling from either result source pulls from the argument source.
--   Each result source only gets the elements pulled at the time, 
--   so if one side pulls all the elements the other side won't get any.
connect_i :: States () m
          => Source m a -> m (Source m a, Source m a)
connect_i = G.connect_i
{-# INLINE connect_i #-}


-- Splitting ------------------------------------------------------------------
-- | Split the given number of elements from the head of a source,
--   returning those elements in a list, and yielding a new source
--   for the rest.
head_i  :: States () m
        => Int -> Source m a -> m ([a], Source m a)

head_i len s0 
        = G.head_i len s0 ()
{-# INLINE head_i #-}


-- | Peek at the given number of elements in the stream, 
--   returning a result stream that still produces them all.
peek_i  :: States () m 
        => Int -> Source m a -> m ([a], Source m a)
peek_i n s0
 = do   (s1, s2) <- G.connect_i s0
        xs       <- G.takeList1 n () s1
        s3       <- G.prepend_i xs s2
        return   (xs, s3)
{-# INLINE peek_i #-}


-- Grouping -------------------------------------------------------------------
-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
-- 
--   Example: groups [4, 4, 4, 3, 3, 1, 1, 1, 4] = [3, 2, 3, 1]
--
groups_i :: (Monad m, Eq a)
         => Source m a -> m (Source m Int)
groups_i = G.groups_i 
{-# INLINE groups_i #-}


-- Packing --------------------------------------------------------------------
-- | Given a stream of flags and a stream of values, produce a new stream
--   of values where the corresponding flag was True. The length of the result
--   is the length of the shorter of the two inputs.
pack_ii  :: Monad m
         => Source m Bool -> Source m a -> m (Source m a)
pack_ii s0 s1 = G.pack_ii s0 s1
{-# INLINE pack_ii #-}


-- Folding --------------------------------------------------------------------
-- | Segmented fold. 
folds_ii :: Monad m
         => (a -> a -> a)    -> a
         -> Source m Int  -> Source m a 
         -> m (Source m a)
folds_ii f z s0 s1 = G.folds_ii f z s0 s1
{-# INLINE folds_ii #-}


-- Watching -------------------------------------------------------------------
-- | Apply a monadic function to every element pulled from a source
--   producing a new source.
watch_i :: Monad m 
        => (a -> m ()) 
        -> Source m a  -> m (Source m a)
watch_i f s0 = G.watch_i (\_ x -> f x) s0
{-# INLINE watch_i #-}


-- | Pass elements to the provided action as they are pushed to the sink.
watch_o :: Monad m 
        => (a -> m ())
        -> Sink m a -> m (Sink m a)
watch_o f s0 = G.watch_o (\_ x -> f x) s0
{-# INLINE watch_o #-}


-- | Like `watch` but doesn't pass elements to another sink.
trigger_o :: Monad m 
          => (a -> m ()) -> m (Sink m a)
trigger_o f  = G.trigger_o () (\_ x -> f x)
{-# INLINE trigger_o #-}


-- Ignorance ------------------------------------------------------------------
-- | A sink that ignores all incoming elements.
--
--   This sink is strict in the elements, so they are demanded before being
--   discarded. Haskell debugging thunks attached to the elements will be demanded.
ignore_o  :: Monad m 
          => m (Sink m a)
ignore_o = G.ignore_o ()
{-# INLINE ignore_o #-}


-- | A sink that drops all data on the floor.
--
--   This sink is non-strict in the elements. 
--   Haskell tracing thinks attached to the elements will *not* be demanded.
abandon_o :: Monad m 
          => m (Sink m a)
abandon_o = G.abandon_o ()
{-# INLINE abandon_o #-}

