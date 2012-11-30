
module Data.Array.Repa.Bulk.Seq
        ( -- * Filling
          fillLinear
        , fillBlock2

          -- * Reduction
        , foldAll
        , foldRange
        , foldInner)
where
import Data.Array.Repa.Bulk.Seq.Chunked
import Data.Array.Repa.Bulk.Seq.Reduction
