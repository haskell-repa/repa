
module Data.Repa.Flow.Chunked.Operator.Folds
        ( folds_i
        , FoldsWorthy)
where
import Data.Repa.Flow.Chunked.Base
import Data.Repa.Flow.States
import Data.Repa.Fusion.Option
import Data.Repa.Array                    as A
import Data.Repa.Eval.Array               as A
import qualified Data.Repa.Flow.Generic   as G

-- | Dictionaries needed to perform a segmented fold.
type FoldsWorthy i m r1 r2 r3 t1 t2 t3 a b
        = ( States i m, Window r1 DIM1 Int, Window r2 DIM1 a
          , Bulk   r1 DIM1 Int, Bulk   r2 DIM1 a
          , Target r1 Int t1,   Target r2 a t2,  Target r3 b t3, Bulk r3 DIM1 b)


-- Folds ----------------------------------------------------------------------
-- | Segmented fold over vectors of segment lengths and input values.
folds_i :: forall i m r1 r2 r3 t1 t2 t3 a b
        .  FoldsWorthy i m r1 r2 r3 t1 t2 t3 a b
        => (a -> b -> b)         -- ^ Worker function.
        -> b                     -- ^ Initial state when folding each segment.
        -> Sources i m r1 Int    -- ^ Segment lengths.
        -> Sources i m r2 a      -- ^ Input elements to fold.
        -> m (Sources i m r3 b)  -- ^ Result elements.

folds_i f z sLens@(G.Sources nLens _)
            sVals@(G.Sources nVals _)
 = do
        -- Arity of the result bundle is the minimum of the two inputs.
        let nFolds = min nLens nVals

        -- Refs to hold partial fold states between chunks.
        refsState    <- newRefs nFolds None2

        -- Refs to hold the current chunk of lengths data for each stream.
        refsLens     <- newRefs nFolds Nothing

        -- Refs to hold the current chunk of vals data for each stream.
        refsVals     <- newRefs nFolds Nothing
        refsValsDone <- newRefs nFolds False

        let pull_folds :: Ix i -> (Vector r3 b -> m ()) -> m () -> m ()
            pull_folds i eat eject
             = do mLens <- folds_loadChunkLens sLens refsLens i
                  mVals <- folds_loadChunkVals sVals refsVals refsValsDone i 

                  case (mLens, mVals) of
                   -- If we couldn't get a chunk for both sides then we can't
                   -- produce anymore results, and the merge is done.
                   (Nothing, _)             -> eject

                   -- We've got a chunk for both sides, time to do some work.
                   (Just cLens, Just cVals) 
                    -> cLens `seq` cVals `seq`
                       do 
                          mState    <- readRefs refsState i

                          let (cResults, sFolds) 
                                = A.folds f z mState cLens cVals

                          folds_update 
                                refsState refsLens refsVals i 
                                cLens cVals sFolds

                          valsDone <- readRefs refsValsDone i

                          -- If we're not producing output while we still
                          -- have segment lengths then we're done.
                          if  A.length cResults == 0
                           && A.length cLens    >= 0
                           && valsDone
                                then eject
                                else eat cResults
            {-# INLINE pull_folds #-} 

        return $ G.Sources nFolds pull_folds
{-# INLINE [1] folds_i #-}


-- Load the current chunk of lengths data.
-- If we already have one in the state then use that, 
-- otherwise try to pull a new chunk from the source.
folds_loadChunkLens 
    :: (States i m, Target r1 Int t1)
    => Sources i m r1 Int
    -> Refs i m (Maybe (Vector r1 Int)) 
    -> Ix i 
    -> m (Maybe (Vector r1 Int))

folds_loadChunkLens (G.Sources _ pullLens) refsLens i
 = do mChunkLens <- readRefs refsLens i
      case mChunkLens of 
       Nothing 
        -> let eatLens_folds chunk
                = writeRefs refsLens i (Just chunk)
               {-# INLINE eatLens_folds #-}

               ejectLens_folds = return ()
               {-# INLINE ejectLens_folds #-}

           in do
               pullLens i eatLens_folds ejectLens_folds
               readRefs refsLens i

       jc@(Just _)
        ->    return jc
{-# NOINLINE folds_loadChunkLens #-}
--  NOINLINE as this doesn't need to be specialized,
--- and we want to hide the case from the simplifier.


-- Grab the current chunk of values data.
-- If we already have one in the state then use that,
-- otherwise try to pull a new chunk from the source.
folds_loadChunkVals 
        :: (States i m, Target r2 a t2)
        => Sources i m r2 a
        -> Refs i m (Maybe (Vector r2 a))
        -> Refs i m Bool
        -> Ix i 
        -> m (Maybe (Vector r2 a))

folds_loadChunkVals (G.Sources _ pullVals) refsVals refsValsDone i
 = do mChunkVals <- readRefs refsVals i
      case mChunkVals of
       Nothing
        -> let  eatVals_folds chunk
                 = writeRefs refsVals i (Just chunk)
                {-# INLINE eatVals_folds #-}

                -- When there are no more values then shim in an 
                -- empty chunk so that we can keep calling A.folds
                -- this is needed when there are zero lengthed
                -- segments on the end of the stream
                ejectVals_folds 
                 = do writeRefs refsVals     i (Just $ vfromList [])
                      writeRefs refsValsDone i True
                {-# INLINE ejectVals_folds #-}

           in do
                pullVals i eatVals_folds ejectVals_folds
                readRefs refsVals i

       jc@(Just _)    
        ->     return jc
{-# NOINLINE folds_loadChunkVals #-}
--  NOINLINE as this doesn't need to be specialized, 
--  and we want to hide the case from the simplifier.


folds_update
        :: ( States i m
           , Window r1 DIM1 Int, Window r2 DIM1 a)
        => Refs i m (Option2 Int b)
        -> Refs i m (Maybe (Vector r1 Int))
        -> Refs i m (Maybe (Vector r2 a))
        -> Ix i 
        -> Vector r1 Int
        -> Vector r2 a
        -> Folds Int Int a b
        -> m ()

folds_update refsState refsLens refsVals i cLens cVals sFolds 
 = do 
        -- Remember state for the final segment.
        writeRefs refsState i 
         $ if _active sFolds 
            then Some2 (_lenSeg sFolds) (_valSeg sFolds)
            else None2

        -- Slice down the lengths chunk to just the elements
        -- that we haven't already consumed. If we've consumed
        -- them all then clear the chunk reference so a new one
        -- will be loaded the next time around.
        let !posLens     = _stateLens sFolds
        let !nLensRemain = A.length cLens - posLens
        writeRefs refsLens i 
         $ if nLensRemain <= 0 
              then Nothing
              else Just $ A.window (Z :. posLens) (Z :. nLensRemain) cLens

        -- Likewise for the values chunk.
        let !posVals     = _stateVals sFolds
        let !nValsRemain = A.length cVals - posVals
        writeRefs refsVals i
         $ if nValsRemain <= 0
              then Nothing
              else Just $ A.window (Z :. posVals) (Z :. nValsRemain) cVals
{-# NOINLINE folds_update #-}
--  NOINLINE because it is only called once per chunk
--  and does not need to be specialised.

