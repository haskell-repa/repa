
module Data.Repa.Chain.Folds
        (foldsC, Folds (..))
where
import Data.Repa.Chain.Base
import Data.Repa.Scalar.Option
import Data.Vector.Fusion.Stream.Size  as S
#include "repa-stream.h"


-- | Segmented fold over vectors of segment lengths and input values.
--
--   The total lengths of all segments need not match the length of the
--   input elements vector. The returned `C.Folds` state can be inspected
--   to determine whether all segments were completely folded, or the 
--   vector of segment lengths or elements was too short relative to the
--   other.
--
foldsC  :: Monad m
        => (a -> b -> m b)        -- ^ Worker function.
        -> b                      -- ^ Initial state when folding rest of segments.
        -> Option3 n Int b        -- ^ Name, length and initial state for first segment.
        -> Chain m sLen (n, Int)  -- ^ Segment names and lengths.
        -> Chain m sVal a         -- ^ Input data to fold.
        -> Chain m (Folds sLen sVal n a b) (n, b)

foldsC   f zN s0 
         (Chain _szLens sLens0 stepLens) 
         (Chain _szVals sVals0 stepVals)
 = Chain S.Unknown (init_foldsC s0) step
 where
        init_foldsC s
         = case s of
            None3           -> Folds sLens0 sVals0 None     0   zN
            Some3 n len acc -> Folds sLens0 sVals0 (Some n) len acc
        {-# NOINLINE init_foldsC #-}
        --  NOINLINE to hide the case match from the simplifier so it
        --  doesn't unswitch it at top-level and duplicate the follow-on code.

        step ss@(Folds sLens sVals nameSeg lenSeg valSeg)
         = case nameSeg of
            -- If we don't have a segment length we need to load the next one.
            None
             -> stepLens sLens >>= \rLens
             -> case rLens of
                 -- We got a segment length, so load it into the state and
                 -- initialise the accumulator.
                 Yield (name, xLen) sLens' 
                  -> return  $ Skip   ss { _stateLens = sLens'
                                         , _nameSeg   = Some name
                                         , _lenSeg    = xLen 
                                         , _valSeg    = zN     }

                 -- Lengths input takes a step.
                 Skip  sLens' 
                  -> return  $ Skip   ss { _stateLens = sLens' }

                 -- We're not currently folding a segment, and no more segment
                 -- lengths are available, so we're done.
                 Done  sLens' 
                  -> return  $ Done   ss { _stateLens = sLens' }

            -- We're currently folding a segment.
            Some name
             -- We've reached the end of the segment, so emit the result.
             |  lenSeg == 0 
             -> return $ Yield (name, valSeg) 
                                        ss { _nameSeg   = None }

             -- We still need more values for this segment.
             |  otherwise
             -> stepVals sVals >>= \rVals
             -> case rVals of
                 -- We got a new value, so accumulate it into the state.
                 Yield xVal sVals'
                  -> f xVal valSeg >>= \rAcc
                  -> return $ Skip    ss { _stateVals = sVals'
                                         , _lenSeg    = lenSeg - 1
                                         , _valSeg    = rAcc }

                 -- Vals input takes a step.
                 Skip sVals'
                  -> return $ Skip    ss { _stateVals = sVals' }

                 -- We're in a non-zero lengthed segment, but haven't got
                 -- all the values, so we're done for now.
                 Done sVals'
                  -> return $ Done    ss { _stateVals = sVals' }
        {-# INLINE_INNER step #-}
{-# INLINE_STREAM foldsC #-}


-- | Return state of a folds operation.
data Folds sLens sVals n a b
        = Folds 
        { -- | State of lengths chain.
          _stateLens        :: !sLens

          -- | State of values chain.
        , _stateVals        :: !sVals

          -- | If we're currently in a segment, then hold its name,
        , _nameSeg          :: !(Option n)

          -- | Length of current segment.
        , _lenSeg           :: !Int

          -- | Accumulated value of current segment.
        , _valSeg           :: !b }
        deriving Show


{-

 -- Defining folds in terms of weave doesn't work because if all the
 -- segment lengths are 0 then we don't want to load any values at all.

 = weaveC work s0 cLens cVals
 where  
        work !ms !mxLen !mxVal 
         = case ms of
            -- If we haven't got a current state then load the next
            -- segment length.
            None2
             -> case mxLen of 
                 None           -> return $ Finish ms MoveNone
                 Some xLen      -> return $ Next (Some2 xLen zN) MoveLeft

            Some2 len acc
             | len == 0         -> return $ Give   acc None2 MoveNone
             | otherwise
             -> case mxVal of
                 None           -> return $ Finish ms MoveNone
                 Some xVal
                  -> do r <- f xVal acc
                        return  $ Next (Some2 (len - 1) r) MoveRight
        {-# INLINE [1] work #-}


-- | Pack the weave state of a folds operation into a `Folds` record, 
--   which has better field names.
packFolds :: Weave sLens Int sVals a (Option2 Int b)
          -> Folds sLens sVals a b

packFolds (Weave stateL elemL _endL stateR elemR _endR mLenAcc)
        = (Folds stateL elemL stateR elemR mLenAcc)
{-# INLINE packFolds #-}
-}
