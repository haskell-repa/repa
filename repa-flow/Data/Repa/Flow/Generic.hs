
-- | Everything flows.
--
--   This module defines generic flows. The other flow types defined
--   in "Data.Repa.Flow.Chunked" and "Data.Repa.Flow.Simple" are
--   specialisations of this generic one.
--
module Data.Repa.Flow.Generic
        ( Sources       (..)
        , Sinks         (..)

          -- * Stream State and Thread Safety

          -- $threadsafety

        , module Data.Repa.Flow.States

          -- * Evaluation
        , drainS
        , drainP
        , consumeS

          -- * Conversion
        , fromList
        , toList1
        , takeList1

        , pushList
        , pushList1

          -- * Stream Indices
        , mapIndex_i
        , mapIndex_o
        , flipIndex2_i
        , flipIndex2_o

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
        , map_i,        map_o
        , smap_i,       smap_o
        , szipWith_ii,  szipWith_io,    szipWith_oi

          -- ** Processing
        , compact_i
        , scan_i
        , indexed_i

          -- ** Connecting
        , dup_oo,       dup_io,         dup_oi
        , connect_i
        , funnel_i
        , funnel_o

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

          -- ** Capturing
        , capture_o
        , rcapture_o

          -- ** Ignorance
        , ignore_o
        , abandon_o

          -- ** Tracing
        , trace_o

          -- * Vector Flow Operators
          -- ** 1-dimensional distribution
        , distribute_o
        , ddistribute_o

          -- ** 2-dimensional distribution
        , distribute2_o
        , ddistribute2_o

          -- ** Shuffling
        , shuffle_o
        , dshuffle_o
        , dshuffleBy_o

          -- ** Chunking
        , chunkOn_i
        , unchunk_i)
where
import Data.Repa.Flow.States
import Data.Repa.Flow.Generic.Base
import Data.Repa.Flow.Generic.Connect
import Data.Repa.Flow.Generic.List
import Data.Repa.Flow.Generic.Map
import Data.Repa.Flow.Generic.Process
import Data.Repa.Flow.Generic.Operator
import Data.Repa.Flow.Generic.Eval
import Data.Repa.Flow.Generic.Array.Distribute
import Data.Repa.Flow.Generic.Array.Shuffle
import Data.Repa.Flow.Generic.Array.Chunk
import Data.Repa.Flow.Generic.Array.Unchunk


-- $threadsafety
--   As most functions in this library produce `IO` actions, thread safety is not
--   guaranteed by their types. 
--
--   It is /not safe/ to concurrently pull from the same stream of a `Sources`
--   bundle, or concurrently push to the same stream of a `Sinks` bundle.
--   Both `Sources` and `Sinks` may hold per-stream state information, and 
--   accessing the same stream concurrently may cause a race condition.
--
--   It is safe to concurrently push or pull from /different/ streams of a bundle,
--   as the state information for each stream is guaranteed to be separate. 
--   Any inter-stream communication is protected by appropriate locks.
--
--   Unless stated otherwise, any worker function passed to a flow operator may
--   be invoked concurrently. For example, if you pass an `IO` action to
--   `trigger_o` then that action may be invoked concurrently.
--
--   In practice, if you use just the bulk operators provided by this library
--   then you won't have a problem. However, if you construct your own 
--   `Sources` or `Sinks` by providing raw @push@, @pull@ and @eject@ functions
--   then you must obey the above rules.
--

