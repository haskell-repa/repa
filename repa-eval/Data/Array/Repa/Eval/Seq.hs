
module Data.Array.Repa.Eval.Seq
        ( -- * Filling
          fillLinear
        , fillBlock2
        , fillCursoredBlock2

          -- * Reduction
        , foldAll
        , foldRange
        , foldInner)
where
import Data.Array.Repa.Eval.Seq.Chunked
import Data.Array.Repa.Eval.Seq.Cursored
import Data.Array.Repa.Eval.Seq.Reduction
