
module Data.Array.Repa.Bulk.Par
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
import Data.Array.Repa.Bulk.Par.Chunked
import Data.Array.Repa.Bulk.Par.Cursored
import Data.Array.Repa.Bulk.Par.Interleaved
import Data.Array.Repa.Bulk.Par.Reduction
