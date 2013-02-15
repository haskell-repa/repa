
-- | Sequential flows provide an incremental version of array fusion that
--   allows the computation to be suspended and resumed at a later time.
--
--   The parallel flow operators are defined in terms of these sequential
--   ones.
--
--   The subset of operators provided are guaranteed to use only a constant
--   amount of space. For example, we don't provide a `dup_ff` operator because
--   if all input elements were pulled from one output before the other,
--   then we would need to buffer the entire input flow. Similarly, we don't 
--   provide a `zip_cc` operator for the dual reason.
--
module Data.Array.Repa.Flow.Seq
        ( FD
        , FS
        , Size          (..)

        -- * Flows
        , Flow          (..)
        , FlowState     (..)
        , joinFlowStates
        , getFlowState
        , startFlow
        , Step1         (..)
        , Step8         (..)
        
        -- * CoFlows
        , CoFlow        (..)
        , CoFlowState   (..)
        , joinCoFlowStates
        , getCoFlowState
        , startCoFlow
        , Snack1        (..)
        , Snack8        (..)

        -- * Conversion
        , flow
        , drain
        , slurp
        , unflowIO

        -- * Map
        , map_f
        , map_c

        -- * Dup
        , dup_cc
        , dup_fc
        , dup_cf

        -- * Zip
        , zip_ff
        , zipWith_ff
        , zipLeft_f
        , zipLeftWith_f

        -- * Construction
        , generate
        , replicate
        , replicatesDirect
        , enumFromN
        , appends


        -- * Projection
        , gather

        -- * Pack
        , packByTag
        , packByFlag
        , filter

        -- * Combine
        , combine2
        , combines2

        -- * Reduction
        , foldl
        , folds
        , sums)
where
import Data.Array.Repa.Flow.Seq.Base
import Data.Array.Repa.Flow.Seq.Flow
import Data.Array.Repa.Flow.Seq.CoFlow
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

