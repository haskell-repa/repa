
module Data.Array.Repa.Bulk.Seq
        ( -- * Filling
          fillLinear
        , fillBlock2
        , fillCursoredBlock2

          -- * Reduction
        , foldAll
        , foldRange
        , foldInner)
where
import Data.Array.Repa.Bulk.Seq.Chunked
import Data.Array.Repa.Bulk.Seq.Cursored
import Data.Array.Repa.Bulk.Seq.Reduction
