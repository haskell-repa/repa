
module Data.Repa.Flow.Chunked.Process
        ( process_i 
        , unfolds_i,    A.StepUnfold(..))
where
import Data.Repa.Flow.Chunked.Base
import qualified Data.Repa.Array.Generic        as A
import qualified Data.Repa.Array.Generic.Index  as A
import qualified Data.Repa.Flow.Generic.Base    as G
#include "repa-flow.h"


-- | Apply a generic stream process to all the streams in a bundle of sources.
process_i
        :: ( G.States i m
           , A.BulkI   lSrc a
           , A.Bulk    lDst b, A.Bulk    lDst (A.Array lDst b)
           , A.TargetI lDst b, A.TargetI lDst (A.Array lDst b))
        => (s -> a -> (s, A.Array lDst b))      -- ^ Worker function.
        -> s                                    -- ^ Initial state.
        ->    Sources i m lSrc a                -- ^ Input sources
        -> m (Sources i m lDst b)               -- ^ Result sources.

process_i f z (G.Sources n pullA)
 = do
        refs    <- G.newRefs n z

        let pull_process i eatB ejectB
             = do s1 <- G.readRefs refs i
                  pullA i (eatA_process s1) ejectA_process

             where eatA_process s1 xA
                    = case A.process A.name f s1 xA of
                        (s1', arrB) 
                         -> do  G.writeRefs refs i s1'
                                eatB arrB
                   {-# INLINE eatA_process #-}

                   ejectA_process 
                    = ejectB
                   {-# INLINE ejectA_process #-}
            {-# INLINE pull_process #-}

        return $ G.Sources n pull_process
{-# INLINE_FLOW process_i #-}


-- | Apply a generic stream process to all the streams in a bundle of sources.
unfolds_i
        :: ( G.States i m
           , A.BulkI   lSrc a
           , A.Bulk    lDst b
           , A.TargetI lDst b)
        => (a -> s -> A.StepUnfold s b)         -- ^ Worker function.
        -> s                                    -- ^ Initial state.
        ->    Sources i m lSrc a                -- ^ Input sources
        -> m (Sources i m lDst b)               -- ^ Result sources.

unfolds_i f z (G.Sources n pullA)
 = do
        refs    <- G.newRefs n z

        let pull_unfolds i eatB ejectB
             = do s1 <- G.readRefs refs i
                  pullA i (eatA_unfolds s1) ejectA_unfolds

             where eatA_unfolds s1 xA
                    = case A.unfolds A.name f s1 xA of
                        (s1', arrB) 
                         -> do  G.writeRefs refs i s1'
                                eatB arrB
                   {-# INLINE eatA_unfolds #-}

                   ejectA_unfolds
                    = ejectB
                   {-# INLINE ejectA_unfolds #-}
            {-# INLINE pull_unfolds #-}

        return $ G.Sources n pull_unfolds
{-# INLINE_FLOW unfolds_i #-}

