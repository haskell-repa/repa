
module Data.Array.Repa.Eval.Par
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
import Data.Array.Repa.Eval.Par.Chunked
import Data.Array.Repa.Eval.Par.Cursored
import Data.Array.Repa.Eval.Par.Interleaved
import Data.Array.Repa.Eval.Par.Reduction
