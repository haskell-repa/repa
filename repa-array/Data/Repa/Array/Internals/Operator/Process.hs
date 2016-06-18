
module Data.Repa.Array.Internals.Operator.Process
        ( process
        , unfolds, StepUnfold (..))
where
import Data.Repa.Array.Internals.Operator.Concat        as A
import Data.Repa.Array.Internals.Layout                 as A
import Data.Repa.Array.Internals.Target                 as A
import Data.Repa.Array.Internals.Bulk                   as A
import Data.Repa.Chain                                  as C
import Data.Repa.Eval.Chain                             as A
import Prelude                                          hiding (concat)
#include "repa-array.h"


-- | Apply a generic stream process to an array.
--
process :: ( BulkI   lSrc a
           , BulkI   lDst b,    Bulk lDst (Array lDst b)
           , TargetI lDst b
           , TargetI lDst (Array lDst b))
        => Name lDst                            -- ^ Name of destination layout.
        -> (s -> a -> (s, Array lDst b))        -- ^ Worker function.
        -> s                                    -- ^ Initial state.
        ->  Array lSrc a                        -- ^ Input array.
        -> (s, Array lDst b)                    -- ^ Result state and array.

process nDst f s0 arr
 = let  
        work_process s x
         = case f s x of
                (s', arr') -> return (s', Just arr')
        {-# INLINE_ARRAY work_process #-}
 
        (arrs, (_, s2))
                = A.unchainToArray nDst
                $ C.scanMaybeC work_process s0 
                $ A.chainOfArray arr

        -- TODO: this concat here is a performance disaster.
        --       Callers using 'process' should be rewritten to use 'unfolds'
   in   (s2, concat nDst arrs)
{-# INLINE_ARRAY process #-}



-- | Apply a generic stream process to an array.
--
unfolds :: ( BulkI   lSrc a
           , TargetI lDst b)
        => Name lDst                            -- ^ Name of destination layout.
        -> (a -> s -> (StepUnfold s b))         -- ^ Worker function.
        -> s                                    -- ^ Initial state.
        -> Array lSrc a                         -- ^ Input array.
        -> (s, Array lDst b)                    -- ^ Result state and array.

unfolds nDst f s0 arr
 = let  
        work_process xa s
         = case f xa s of
                mxb -> return mxb
        {-# INLINE_ARRAY work_process #-}

        (arr', (_, s2, _))
                = A.unchainToArray nDst
                $ C.unfoldsC work_process s0 
                $ A.chainOfArray arr

   in   (s2, arr')
{-# INLINE_ARRAY unfolds #-}

