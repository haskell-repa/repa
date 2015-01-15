
-- | Everything flows.
module Data.Repa.Flow.Generic
        ( module Data.Repa.Flow.States
        , Sources       (..)
        , Sinks         (..)

          -- * Evaluation
        , drain

          -- * Conversion
        , fromList
        , toList1
        , takeList1

          -- * Finalizers
        , finalize_i
        , finalize_o

          -- * Flow Operators
          -- ** Projection
        , project_i
        , project_o

          -- ** Constructors
        , repeat_i
        , replicate_i
        , prepend_i,    prependOn_i

          -- ** Mapping
        , smap_i,       smap_o

          -- ** Connecting
        , dup_oo,       dup_io,         dup_oi
        , connect_i

          -- ** Splitting
        , head_i

          -- ** Grouping
        , groups_i

          -- ** Packing
        , pack_ii

          -- ** Folding
        , folds_ii

          -- ** Watching
        , watch_i
        , watch_o
        , trigger_o

          -- ** Ignorance
        , discard_o
        , ignore_o

          -- * Vector Flow Operators
          -- ** Distribution
        , distributes_o
        , ddistributes_o

          -- * Flow IO
          -- ** Sourcing Records
        , fileSourcesRecords,   hSourcesRecords

          -- ** Sourcing Chunks
        , fileSourcesChunks,    hSourcesChunks

          -- ** Sourcing Bytes
        , fileSourcesBytes,     hSourcesBytes

          -- ** Sinking Bytes
        , fileSinksBytes,       hSinksBytes)
where
import Data.Repa.Flow.States
import Data.Repa.Flow.Generic.Base
import Data.Repa.Flow.Generic.List
import Data.Repa.Flow.Generic.Operator
import Data.Repa.Flow.Generic.Vector
import Data.Repa.Flow.Generic.Eval
import Data.Repa.Flow.Generic.IO

