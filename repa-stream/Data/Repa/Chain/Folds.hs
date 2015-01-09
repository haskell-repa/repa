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
        -> b                    -- ^ Initial state when folding first segment.
        -> b                    -- ^ Initial state when folding rest of segments.
        -> MChain m sLen Int    -- ^ Segment lengths.
        -> MChain m sVal a      -- ^ Input data to fold.
        -> MChain m (Weave sLen Int sVal a (Maybe Int, b)) b

foldsC f z0 zN cLens cVals 
 = weaveC work (Nothing, z0) cLens cVals
 where  
        work (mLen, acc) !xLen !xVal 
         = case mLen of
            Nothing      
             -> return $ Next (Just xLen, acc) MoveNone

            Just len
             | len == 0  
             -> return $ Give acc (Nothing, zN)    MoveLeft

             | otherwise 
             -> do r  <- f xVal acc
                   return $ Next (Just (len - 1), r) MoveRight
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

          -- | Length of current segment.
        , foldsSegAcc           :: Maybe Int

          -- | Accumulator for current segment.
        , foldsValAcc           :: b }
        deriving Show


-- | Pack the weave state of a folds operation into a `Folds` record, 
--   which has better field names.
packFolds :: Weave sLens Int sVals a (Maybe Int, b)
          -> Folds sLens sVals a b

packFolds (Weave stateL elemL stateR elemR (mLen, acc))
        = (Folds stateL elemL stateR elemR mLen acc)
{-# INLINE packFolds #-}

