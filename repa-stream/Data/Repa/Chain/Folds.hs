{-# LANGUAGE CPP #-}
module Data.Repa.Chain.Folds
        (foldsC, Folds (..), packFolds)
where
import Data.Repa.Chain.Base
import Data.Repa.Chain.Weave
#include "vector.h"


foldsC  :: Monad m
        => (a -> b -> m b)
        -> b
        -> MChain m sLen Int
        -> MChain m sVal a
        -> MChain m (Weave sLen Int sVal a (Maybe Int, b)) b

foldsC f z cLens cVals 
 = weaveC work (Nothing, z) cLens cVals
 where  
        work (mLen, acc) !xLen !xVal 
         = case mLen of
            Nothing      
             -> return $ Next (Just xLen, acc) MoveNone

            Just len
             | len == 0  
             -> return $ Give acc (Nothing, z)    MoveLeft

             | otherwise 
             -> do r  <- f xVal acc
                   return $ Next (Just (len - 1), r) MoveRight
        {-# INLINE work #-}
{-# INLINE_STREAM foldsC #-}


-------------------------------------------------------------------------------
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

