
-- | Replicate operations on arrays.
module Data.Repa.Array.Internals.Operator.Replicate
        ( replicates )
where
import Data.Repa.Array.Internals.Layout         as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Eval.Stream                    as A
import Data.Repa.Fusion.Unpack                  as A
import Data.Repa.Stream                         as S
#include "repa-array.h"


-- | Segmented replicate.
replicates 
        :: ( BulkI lSrc (Int, a), TargetI lDst a
           , Unpack (Buffer lDst a) t)
        => Name lDst
        -> Array lSrc (Int, a)
        -> Array lDst a

replicates nDst arr
        = A.unstreamToArray nDst
        $ S.replicatesS
        $ A.streamOfArray arr
{-# INLINE_ARRAY replicates #-}

