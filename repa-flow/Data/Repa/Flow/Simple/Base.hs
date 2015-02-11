
module Data.Repa.Flow.Simple.Base
        ( Source, Sink
        , finalize_i
        , finalize_o
        , wrapI_i
        , wrapI_o)
where
import Data.Repa.Flow.States
import qualified Data.Repa.Flow.Generic as G
#include "repa-flow.h"


-- | Source consisting of a single stream.
type Source m e = G.Sources () m e

-- | Sink consisting of a single stream.
type Sink   m e = G.Sinks   () m e


-- Finalizers -----------------------------------------------------------------
-- | Attach a finalizer to a source.
--
--   The finalizer will be called the first time a consumer of that stream
--   tries to pull an element when no more are available.
--
--   The provided finalizer will be run after any finalizers already
--   attached to the source.
--
finalize_i
        :: States () m
        => m ()
        -> Source m a -> m (Source m a)

finalize_i f s0 = G.finalize_i (\_ -> f) s0
{-# INLINE finalize_i #-}


-- | Attach a finalizer to a sink.
--
--   The finalizer will be called the first time the stream is ejected.
--
--   The provided finalizer will be run after any finalizers already
--   attached to the sink.
--
finalize_o
        :: States () m
        => m ()
        -> Sink m a -> m (Sink m a)

finalize_o f s0 = G.finalize_o (\_ -> f) s0
{-# INLINE finalize_o #-}


-- Wrapping -------------------------------------------------------------------
wrapI_i  :: G.Sources Int m e -> Maybe (Source m e)
wrapI_i (G.Sources n pullX)
 | n /= 1       = Nothing
 | otherwise    
 = let  pullX' _ eat eject 
         = pullX 0 eat eject 
        {-# INLINE pullX' #-}
   in   Just $ G.Sources () pullX'
{-# INLINE_FLOW wrapI_i #-}


wrapI_o  :: G.Sinks Int m e -> Maybe (Sink m e)
wrapI_o (G.Sinks n eatX ejectX)
 | n /= 1       = Nothing
 | otherwise    
 = let  eatX' _ x       = eatX   0 x
        ejectX' _       = ejectX 0
   in   Just $ G.Sinks () eatX' ejectX'
{-# INLINE_FLOW wrapI_o #-}

