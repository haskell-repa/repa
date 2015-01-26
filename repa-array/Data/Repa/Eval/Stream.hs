{-# LANGUAGE CPP #-}

-- | Evaluation of `Stream`s into bulk arrays.
module Data.Repa.Eval.Stream
        (stream)
where
import Data.Repa.Array.Shape                            as R
import Data.Repa.Array.Internals.Bulk                   as R
import qualified Data.Vector.Fusion.Stream.Monadic      as S
#include "repa-stream.h"


-- | Convert a `Vector` to a `Stream`.
stream  :: (Monad m, Bulk r DIM1 a)
        => R.Vector r a
        -> S.Stream m a
stream vec
        = S.generate (R.size $ R.extent vec)
                     (\i -> R.index vec (Z :. i))
{-# INLINE_STREAM stream #-}
