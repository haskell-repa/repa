
module Data.Repa.Flow.Chunked.Map
        ( smap_i,       smap_o
        , szipWith_ii)
where
import Data.Repa.Flow.Chunked.Base
import Data.Repa.Flow.States
import Data.Repa.Array                          as A
import Data.Repa.Eval.Array                     as A
import qualified Data.Repa.Flow.Generic         as G
#include "repa-flow.h"


-- | Map a function over elements pulled from a source.
smap_i   :: (Flow i m l1 a, A.TargetI l2 b)
        => (i -> a -> b) -> Sources i m l1 a -> m (Sources i m l2 b)
smap_i f s0 = G.smap_i (\i c -> A.computeS name $ A.map (f i) c) s0
{-# INLINE smap_i #-}


-- | Map a function over elements pushed into a sink.
smap_o  :: (Flow i m l1 a, A.TargetI l2 b)
        => (i -> a -> b) -> Sinks i m l2 b -> m (Sinks i m l1 a)
smap_o f s0 = G.smap_o (\i c -> A.computeS name $ A.map (f i) c) s0
{-# INLINE smap_o #-}


-- | Combine the elements of two flows with the given function.
szipWith_ii 
        :: ( Ord i, States i m
           , BulkI   lSrc1 a, BulkI lSrc2 b
           , TargetI lDst c
           , Windowable lSrc1 a, Windowable lSrc2 b)
        => Name lDst
        -> (i -> a -> b -> c)
        -> Sources i m lSrc1 a -> Sources i m lSrc2 b
        -> m (Sources i m lDst c)

szipWith_ii nDst f (G.Sources nA pullA) (G.Sources nB pullB)
 = do
        let nC  = min nA nB

        -- Refs to hold leftover pieces of chunks.
        bitsA   <- newRefs nC Nothing
        bitsB   <- newRefs nC Nothing

        let pullC i eatC ejectC 
             | not $ check i nC = ejectC
             | otherwise        = getA
             where
                getA 
                 = do   mA              <- readRefs bitsA i
                        case mA of
                         Just chunkA    -> getB    chunkA
                         Nothing        -> pullA i getB   ejectC
                {-# INLINE getA #-}

                getB chunkA
                 = do   mB              <- readRefs bitsB i
                        case mB of
                         Just chunkB    -> zipAB   chunkA chunkB
                         Nothing        -> pullB i (zipAB chunkA) ejectC
                {-# INLINE getB #-}

                zipAB chunkA chunkB
                 = do   
                        let !lenA  = A.length chunkA
                        let !lenB  = A.length chunkB
                        let !lenC  = min lenA lenB

                        -- Split the chunks into the bits that we will zip
                        -- in this round, and the bits that we will leave 
                        -- until later.
                        let !hereA = A.window 0 lenC chunkA
                        let !restA = A.window lenC (lenA - lenC) chunkA

                        let !hereB = A.window 0 lenC chunkB
                        let !restB = A.window lenC (lenB - lenC) chunkB

                        -- Zip the common parts we have now.
                        let Just !hereC = A.map2S nDst (f i) hereA hereB

                        (if A.length restA > 0
                          then writeRefs bitsA i (Just restA)
                          else writeRefs bitsA i Nothing)

                        (if A.length restB > 0
                          then writeRefs bitsB i (Just restB)
                          else writeRefs bitsB i Nothing)

                        eatC hereC
                {-# INLINE zipAB #-}
            {-# INLINE pullC #-}

        return $ G.Sources nC pullC
{-# INLINE_FLOW szipWith_ii #-}

