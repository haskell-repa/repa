
module Data.Repa.Flow.Chunked.Operator.Folds
        (folds_i)
where
import Data.Repa.Flow.Chunked.Base
import Data.Repa.Flow.States
import Data.Repa.Array                    as A
import Data.Repa.Eval.Array               as A
import qualified Data.Repa.Flow.Generic   as G

-- TODO: extract out flow framework to resuse for other weave operators.

-- Folds ------------------------------------------------------------------------------------------
-- | Segmented fold over vectors of segment lengths and input values.
--
--   The total lengths of all segments need not match the length of the
--   input elements vector. The returned `C.Folds` state can be inspected
--   to determine whether all segments were completely folded, or the 
--   vector of segment lengths or elements was too short relative to the
--   other.
--
folds_i :: forall i m r1 r2 r3 a b
        .  (States i m, Window r1 DIM1 Int, Window r2 DIM1 a, Target r3 b)
        => (a -> b -> b)        -- ^ Worker function.
        -> b                    -- ^ Initial state when folding each segment.
        -> Sources i m r1 Int   -- ^ Segment lengths.
        -> Sources i m r2 a     -- ^ Input elements to fold.
        -> m (Sources i m r3 b) -- ^ Result elements.

folds_i f z (G.Sources nLens pullLens)
            (G.Sources nVals pullVals)
 = do
        let nFolds = min nLens nVals

        -- Refs to hold partial fold states between chunks.
        refsState  <- newRefs nFolds Nothing

        -- Refs to hold the current chunk of lengths data for each stream.
        refsLens   <- newRefs nFolds Nothing

        -- Refs to hold the current chunk of vals data for each stream.
        refsVals   <- newRefs nFolds Nothing

        let pull_folds :: Ix i -> (Vector r3 b -> m ()) -> m () -> m ()
            pull_folds i eat eject
             = do mLens <- loadChunkLens i
                  mVals <- loadChunkVals i

                  case (mLens, mVals) of
                   -- If we couldn't get a chunk for both sides then we can't
                   -- produce anymore results, and the merge is done.
                   (Nothing, _)            -> eject
                   (_,       Nothing)      -> eject 

                   -- We've got a chunk for both sides, time to do some work.
                   (Just cLens, Just cVals) 
                    -> cLens `seq` cVals `seq`
                       do 
                          mState    <- readRefs refsState i
                          let (cResults, sFolds) 
                                    = A.folds f z mState cLens cVals

                          -- Remember state for the final segment.
                          writeRefs refsState i (foldsSegAcc sFolds)

                          -- Slice down the lengths chunk to just the elements
                          -- that we haven't already consumed. If we've consumed
                          -- them all then clear the chunk reference so a new one
                          -- will be loaded the next time around.
                          let !posLens     = foldsLensState sFolds
                          let !nLensRemain = A.length cLens - posLens
                          writeRefs refsLens i 
                           $ if nLensRemain <= 0 
                                then Nothing
                                else Just $ A.window (Z :. posLens) (Z :. nLensRemain) cLens

                          -- Likewise for the values chunk.
                          let !posVals     = foldsValsState sFolds
                          let !nValsRemain = A.length cVals - posVals
                          writeRefs refsVals i
                           $ if nValsRemain <= 0
                                then Nothing
                                else Just $ A.window (Z :. posVals) (Z :. nValsRemain) cVals

                          -- Eat the result
                          eat cResults
            {-# INLINE pull_folds #-} 

            -- Load the current chunk of lengths data.
            -- If we already have one in the state then use that, 
            -- otherwise try to pull a new chunk from the source.
            loadChunkLens :: Ix i -> m (Maybe (Vector r1 Int))
            loadChunkLens i
             = do mChunkLens <- readRefs refsLens i
                  case mChunkLens of 
                   Nothing 
                    -> let eatLens_folds :: Vector r1 Int -> m ()
                           eatLens_folds chunk
                            = writeRefs refsLens i (Just chunk)
                           {-# INLINE eatLens_folds #-}

                           ejectLens_folds = return ()
                           {-# INLINE ejectLens_folds #-}

                       in do
                           pullLens i eatLens_folds ejectLens_folds
                           readRefs refsLens i

                   jc@(Just _)
                    ->     return jc
            {-# INLINE loadChunkLens #-}

            -- Grab the current chunk of values data.
            -- If we already have one in the state then use that,
            -- otherwise try to pull a new chunk from the source.
            loadChunkVals :: Ix i -> m (Maybe (Vector r2 a))
            loadChunkVals i
             = do mChunkVals <- readRefs refsVals i
                  case mChunkVals of
                   Nothing
                    -> let eatVals_folds chunk
                            = writeRefs refsVals i (Just chunk)
                           {-# INLINE eatVals_folds #-}

                           ejectVals_folds = return ()
                           {-# INLINE ejectVals_folds #-}

                       in do
                           pullVals i eatVals_folds ejectVals_folds
                           readRefs refsVals i

                   jc@(Just _)    
                    ->     return jc
            {-# INLINE loadChunkVals #-}

        return $ G.Sources nFolds pull_folds
{-# INLINE [1] folds_i #-}

