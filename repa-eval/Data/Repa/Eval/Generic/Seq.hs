
-- | Generic sequential array computation operators.
module Data.Repa.Eval.Generic.Seq
        ( -- * Filling
          fillLinear
        , fillBlock2
        , fillCursoredBlock2

          -- * Reduction
        , foldAll
        , foldRange
        , foldInner)
where
import Data.Repa.Eval.Generic.Seq.Chunked
import Data.Repa.Eval.Generic.Seq.Cursored
import Data.Repa.Eval.Generic.Seq.Reduction
