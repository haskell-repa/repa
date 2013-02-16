
-- | Sequential flows provide an incremental version of array fusion that
--   allows the computation to be suspended and resumed at a later time.
--
--   The operators provided are guaranteed to use only a constant amount of space.
--   For example, we provide a `dup_oo` operator but no `dup_ii` because if all
--   input elements were pulled from one of the out-flows before the other, then
--   we would need to buffer the entire input flow. Similarly, we don't provide a
--   `zip_oo` operator for the dual reason.
--
--   The parallel flow operators are defined in terms of these sequential
--   ones.
--
module Data.Array.Repa.Flow.Seq
        ( FD
        , FS
        , Size          (..)

        -- * Sources
        , Source          (..)
        , SourceState     (..)
        , joinSourceStates
        , getSourceState
        , startSource
        , Step1         (..)
        , Step8         (..)
        
        -- * Sinks
        , Sink        (..)
        , SinkState   (..)
        , joinSinkStates
        , getSinkState
        , startSink
        , Snack1        (..)
        , Snack8        (..)

        -- * Conversion
        , flow
        , drain
        , slurp
        , unflowIO

        -- * Map
        , map_i
        , map_o

        -- * Dup
        , dup_oo
        , dup_io
        , dup_oi

        -- * Zip
        , zip_ii
        , zipWith_ii
        , zipLeft_i
        , zipLeftWith_i

        -- * Construction
        , generate_i
        , replicate_i
        , replicates_bi
        , enumFromN_i
        , appends_bb

        -- * Projection
        , gather_bi

        -- * Pack
        , packByTag_i
        , packByFlag_i
        , filter_i

        -- * Combine
        , combine2_iii
        , combines2_iii

        -- * Reduction
        , foldl_i
        , folds_ii
        , sums_ii)
where
import Data.Array.Repa.Flow.Seq.Base
import Data.Array.Repa.Flow.Seq.Source
import Data.Array.Repa.Flow.Seq.Sink
import Data.Array.Repa.Flow.Seq.Operator.Slurp
import Data.Array.Repa.Flow.Seq.Operator.Dup
import Data.Array.Repa.Flow.Seq.Operator.Map
import Data.Array.Repa.Flow.Seq.Operator.Zip
import Data.Array.Repa.Flow.Seq.Operator.Generate
import Data.Array.Repa.Flow.Seq.Operator.Append
import Data.Array.Repa.Flow.Seq.Operator.Pack
import Data.Array.Repa.Flow.Seq.Operator.Combine
import Data.Array.Repa.Flow.Seq.Operator.Project
import Data.Array.Repa.Flow.Seq.Operator.Fold
import Prelude  hiding (map, zip, zipWith, unzip, foldl, filter, replicate, take)

