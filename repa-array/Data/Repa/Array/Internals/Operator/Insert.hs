
-- | Insertion operations on arrays.
module Data.Repa.Array.Internals.Operator.Insert
        (insert)
where
import Data.Repa.Array.Generic.Index            as A
import Data.Repa.Array.Meta.Tuple               as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Fusion.Unpack                  as A
import Data.Repa.Eval.Stream                    as A
import qualified Data.Repa.Stream               as S
#include "repa-array.h"


-- | Insert elements produced by the given function in to an array.
insert  :: ( BulkI lSrc a, TargetI lDst a
           , Unpack (Buffer lDst a) t0)
        => Name lDst            -- ^ Name of destination layout.
        -> (Int -> Maybe a)     -- ^ Produce an element for this index.
        -> Array lSrc a         -- ^ Array of source elements.
        -> Array lDst a

insert nDst fNew aSrc
        = A.unstreamToArray nDst
        $ S.insertS fNew 
        $ A.streamOfArray   aSrc
{-# INLINE_ARRAY insert #-}

