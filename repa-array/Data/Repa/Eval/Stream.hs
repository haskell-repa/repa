{-# LANGUAGE CPP #-}

-- | Interface with stream fusion.
module Data.Repa.Eval.Stream
        (streamOfArray)
where
import Data.Repa.Array.Index                            as A
import Data.Repa.Array.Internals.Bulk                   as A
import qualified Data.Vector.Fusion.Stream.Monadic      as S
#include "repa-array.h"


-- | Produce a `Stream` for the elements of the given array.
streamOfArray  
        :: (Monad m, Bulk l a, Index l ~ Int)
        => A.Array  l a
        -> S.Stream m a

streamOfArray vec
        = S.generate (A.length vec)
                     (\i -> A.index vec i)
{-# INLINE_STREAM streamOfArray #-}
