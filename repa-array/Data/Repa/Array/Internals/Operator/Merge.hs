
-- | Merge operations on arrays.
module Data.Repa.Array.Internals.Operator.Merge
        ( merge
        , mergeMaybe)
where
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Layout         as A
import Data.Repa.Stream                         as S
import Data.Repa.Eval.Stream                    as A
import Data.Repa.Fusion.Unpack                  as A
import qualified Data.Vector.Fusion.Stream      as S
#include "repa-array.h"


-- | Merge two sorted key-value streams.
merge   :: ( Ord k
           , BulkI l1 (k, a), BulkI l2 (k, b)
           , TargetI lDst (k, c)
           , Unpack (Buffer lDst (k, c)) t0)
        => Name lDst            -- ^ Name of destination layout.
        -> (k -> a -> b -> c)   -- ^ Combine two values with the same key.
        -> (k -> a -> c)        -- ^ Handle a left value without a right value.
        -> (k -> b -> c)        -- ^ Handle a right value without a left value.
        -> Array l1   (k, a)    -- ^ Array of keys and left values.
        -> Array l2   (k, b)    -- ^ Array of keys and right values.
        -> Array lDst (k, c)    -- ^ Array of keys and results.

merge nDst fBoth fLeft fRight arrA arrB
        = A.unstreamToArray nDst 
        $ S.mergeS fBoth fLeft fRight 
                (A.streamOfArray arrA)
                (A.streamOfArray arrB)
{-# INLINE_ARRAY merge #-}


-- | Like `merge`, but only produce the elements where the worker functions
--   return `Just`.
mergeMaybe 
        :: ( Ord k
           , BulkI l1 (k, a), BulkI l2 (k, b)
           , TargetI lDst (k, c)
           , Unpack (Buffer lDst (k, c)) t0)
        => Name lDst
        -> (k -> a -> b -> Maybe c) -- ^ Combine two values with the same key.
        -> (k -> a -> Maybe c)      -- ^ Handle a left value without a right value.
        -> (k -> b -> Maybe c)      -- ^ Handle a right value without a left value.
        -> Array l1   (k, a)        -- ^ Array of keys and left values.
        -> Array l2   (k, b)        -- ^ Array of keys and right values.
        -> Array lDst (k, c)        -- ^ Array of keys and results.

mergeMaybe nDst fBoth fLeft fRight arrA arrB
        = A.unstreamToArray nDst
        $ catMaybesS
        $ S.map  munge_mergeMaybe
        $ mergeS fBoth fLeft fRight
                (A.streamOfArray arrA)
                (A.streamOfArray arrB)

        where   munge_mergeMaybe (_k, Nothing)   = Nothing
                munge_mergeMaybe (k,  Just x)    = Just (k, x)
                {-# INLINE munge_mergeMaybe #-}
{-# INLINE_ARRAY mergeMaybe #-}


