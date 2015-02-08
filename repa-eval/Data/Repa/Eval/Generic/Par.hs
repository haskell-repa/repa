
-- | Generic parallel array computation operators.
module Data.Repa.Eval.Generic.Par
        ( -- * Filling
          fillChunked
        , fillChunkedIO
        , fillBlock2
        , fillInterleaved
        , fillCursoredBlock2

          -- * Reduction
        , foldAll
        , foldInner)
where
import Data.Repa.Eval.Generic.Par.Chunked
import Data.Repa.Eval.Generic.Par.Cursored
import Data.Repa.Eval.Generic.Par.Interleaved
import Data.Repa.Eval.Generic.Par.Reduction
