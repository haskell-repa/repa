{-# LANGUAGE CPP #-}
module Data.Repa.Chain.Folds
        (foldsC, Folds (..), packFolds)
where
import Data.Repa.Chain.Base
import Data.Repa.Chain.Weave
#include "vector.h"


-- | Segmented fold over vectors of segment lengths and input values.
--
--   The total lengths of all segments need not match the length of the
--   input elements vector. The returned `C.Folds` state can be inspected
--   to determine whether all segments were completely folded, or the 
--   vector of segment lengths or elements was too short relative to the
--   other.
--
foldsC  :: Monad m
        => (a -> b -> m b)      -- ^ Worker function.
        -> b                    -- ^ Initial state when folding rest of segments.
        -> Maybe (Int, b)       -- ^ Length and initial state for first segment.
        -> Chain m sLen Int     -- ^ Segment lengths.
        -> Chain m sVal a       -- ^ Input data to fold.
        -> Chain m (Weave sLen Int sVal a (Maybe (Int, b))) b

foldsC    f zN s0 cLens cVals 
 = weaveC work s0 cLens cVals
 where  
        work !ms !xLen !xVal 
         = case ms of
            Nothing      
             ->    return $ Next (Just (xLen, zN)) MoveNone

            Just (len, acc)
             -> len `seq` acc `seq`
                if len == 0  
                 then return $ Give acc Nothing MoveLeft
                 else do r  <- f xVal acc
                         return $ Next (Just (len - 1, r))  MoveRight
        {-# INLINE work #-}
{-# INLINE_STREAM foldsC #-}


-- | Return state of a folds operation.
data Folds sLens sVals a b
        = Folds 
        { -- | State of lengths chain.
          foldsLensState        :: !sLens

          -- | Length of current segment.
        , foldsLensCur          :: !(Maybe Int)

          -- | State of values chain.
        , foldsValsState        :: !sVals

          -- | Current value being processed.
        , foldsValsCur          :: Maybe a

          -- | Number of elements remaining to fold in the current
          --   segment, and current accumulator, or `Nothing` if we're
          --   not in a segment.
        , foldsSegAcc           :: Maybe (Int, b) }
        deriving Show


-- | Pack the weave state of a folds operation into a `Folds` record, 
--   which has better field names.
packFolds :: Weave sLens Int sVals a (Maybe (Int, b))
          -> Folds sLens sVals a b

packFolds (Weave stateL elemL stateR elemR mLenAcc)
        = (Folds stateL elemL stateR elemR mLenAcc)
{-# INLINE packFolds #-}

