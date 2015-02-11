
-- | Everything flows.
--
--   This module defines generic flows. The other flow types defined
--   in "Data.Repa.Flow.Chunked" and "Data.Repa.Flow.Simple" are
--   specialisations of this generic one.
--
module Data.Repa.Flow.Generic
        ( Sources       (..)
        , Sinks         (..)

          -- * States and Thread Safety

          -- $threadsafety

        , module Data.Repa.Flow.States

          -- * Evaluation
        , drainS
        , drainP

          -- * Conversion
        , fromList
        , toList1
        , takeList1

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
          -- ** 1-dimensional distribution
        , distribute_o
        , ddistribute_o

          -- ** 2-dimensional distribution
        , distribute2_o
        , ddistribute2_o

          -- ** Shuffling
        , shuffle_o)
where
import Data.Repa.Flow.States
import Data.Repa.Flow.Generic.Base
import Data.Repa.Flow.Generic.List
import Data.Repa.Flow.Generic.Operator
import Data.Repa.Flow.Generic.Vector
import Data.Repa.Flow.Generic.Shuffle
import Data.Repa.Flow.Generic.Eval


-- $threadsafety
--   As most functions in this library produce `IO` actions, thread safety is not
--   guaranteed by their types. In the current version of the library story is as
--   follows:
--
--    It is /not safe/ to pull from the same stream of a `Sources` bundle 
--     concurrently. The `Source` may hold per-stream state information which
--     is updated each time it is pulled, and pulling the same stream concurrently
--     will cause a race.
--
--    It is safe to pull from /different/ streams of a `Sources` bundle 
--     concurrently. The state information for each stream of a source is
--     guaranteed to be separate.
--
--    It is safe to push to any stream of a `Sinks` bundle concurrently,
--     including pushing to different streams or the same stream at the same time.
--     The state information for each stream in a `Sinks` bundle is protected
--     by appropriate locks.
--
--    Unless stated otherwise, it is safe to construct multiple
--     `Sources` and `Sinks` concurrently. For example, you can apply
--     `map_o` and `folds_i` concurrently, or apply multiple instances of `map_o`.
--     
--   In practice, if stick with the bulk operators provided by this library,
--   and invoke them from a single threaded program, then you won't have a problem.
--   However, if you construct your own `Sources` and `Sinks` by providing
--   raw push, pull and eject functions then you must obey the above rules.
--   In a future version we may also require that concurrently pulling from the
--   same stream of a `Sources` bundle is also thread safe.
--

