
module Data.Repa.Array.Internals.Operator.Reduce
        (foldl)
where
import Data.Repa.Array.Index                            as A
import Data.Repa.Array.Internals.Bulk                   as A
import Data.Repa.Eval.Stream                            as A
import qualified Data.Vector.Fusion.Stream              as S
import Prelude                                          as P hiding (foldl)
#include "repa-array.h"


-- | Left fold of all elements in an array, sequentially.
foldl   :: (Bulk l b, Index l ~ Int)
        => (a -> b -> a) -> a -> Array l b -> a

foldl f z arr
        = S.foldl f z 
        $ streamOfArray arr
{-# INLINE foldl #-}
