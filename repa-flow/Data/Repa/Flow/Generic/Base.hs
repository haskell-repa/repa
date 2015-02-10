
module Data.Repa.Flow.Generic.Base
        ( module Data.Repa.Flow.States
        , Sources       (..)
        , Sinks         (..)
        , mapIndex_i
        , mapIndex_o
        , finalize_i
        , finalize_o)
where
import Data.Repa.Flow.States
import Control.Monad
#include "repa-stream.h"

-- | A bundle of stream sources, indexed by a value of type @i@,
--   in some monad @m@, returning elements of type @e@.
--
--   Elements can be pulled from each stream in the bundle individually.
--
data Sources i m e
        = Sources
        { -- | Number of sources in this bundle.
          sourcesArity  :: i

          -- | Function to pull data from a bundle. 
          --   Give it the index of the desired stream, a continuation that 
          --   accepts an element, and a continuation to invoke when no more
          --   elements will ever be available.
        , sourcesPull   :: i -> (e -> m ()) -> m () -> m () }


-- | A bundle of stream sinks, indexed by a value of type @i@, 
--   in some monad @m@, returning elements of type @e@.
--
--   Elements can be pushed to each stream in the bundle individually.
--
data Sinks   i m e
        = Sinks
        { -- | Number of sources in the bundle.
          sinksArity    :: i

          -- | Push an element to one of the streams in the bundle.
        , sinksPush     :: i -> e -> m ()

          -- | Signal that no more elements will ever be available for this
          --   sink.
        , sinksEject    :: i -> m () }


-------------------------------------------------------------------------------
-- | Transform the stream indexes of a bundle of sources.
mapIndex_i 
        :: Monad m
        => (i1 -> i2)           -- ^ Convert index to new version.
        -> (i2 -> i1)           -- ^ Convert index from new version.
        -> Sources i1 m a
        -> m (Sources i2 m a)

mapIndex_i to from (Sources n pullX)
 = return $ Sources (to n) pull_mapIndex
 where 
        pull_mapIndex i eat eject
         = pullX (from i) eat eject
        {-# INLINE pull_mapIndex #-}
{-# INLINE_FLOW mapIndex_i #-}


-- | Transform the stream indexes of a bundle of sinks.
mapIndex_o 
        :: Monad m
        => (i1 -> i2)           -- ^ Convert index to new version.
        -> (i2 -> i1)           -- ^ Convert index from new version.
        -> Sinks i1 m a
        -> m (Sinks i2 m a)

mapIndex_o to from (Sinks n pushX ejectX)
 = return $ Sinks (to n) push_mapIndex eject_mapIndex
 where 
        push_mapIndex i x = pushX (from i) x
        {-# INLINE push_mapIndex  #-}

        eject_mapIndex i  = ejectX (from i)
        {-# INLINE eject_mapIndex #-}
{-# INLINE_FLOW mapIndex_o #-}


-------------------------------------------------------------------------------
-- | Attach a finalizer to bundle of sources.
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
        => (i -> m ())
        -> Sources i m a -> m (Sources i m a)

finalize_i f (Sources n pull)
 = do
        refs    <- newRefs n False

        let pull_finalize i eat eject
             = pull i eat eject_finalize
             where
                eject_finalize 
                 = do   eject
                        done <- readRefs refs i
                        when (not done)
                         $ do f i
                              writeRefs refs i False
                {-# INLINE eject_finalize #-}
            {-# INLINE pull_finalize #-}

        return  $ Sources n pull_finalize
{-# INLINE_FLOW finalize_i #-}


-- | Attach a finalizer to a bundle of sinks.
--
--   For each stream in the bundle, the finalizer will be called the first
--   time that stream is ejected. 
--
--   The provided finalizer will be run after any finalizers already
--   attached to the sink.
--
finalize_o
        :: States i m
        => (i -> m ())
        -> Sinks i m a -> m (Sinks i m a)

finalize_o f  (Sinks n push eject)
 = do
        refs    <- newRefs n False

        let eject_finalize i 
             = do eject i
                  done <- readRefs refs i
                  when (not done)
                   $ do f i
                        writeRefs refs i False
            {-# INLINE eject_finalize #-}

        return $ Sinks n push eject_finalize
{-# INLINE_FLOW finalize_o #-}

