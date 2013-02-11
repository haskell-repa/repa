
-- | Sequential flows provide an incremental version of array fusion that
--   allows the computation to be suspended and resumed at a later time.
module Data.Array.Repa.Flow.Seq
        ( FD, FS

        -- * Flows
        , Flow   (..)
        , Step1  (..)
        , Step8  (..)
        
        -- * CoFlows
        , CoFlow (..)
        , Snack1 (..)
        , Snack8 (..)

        -- * Conversion
        , flow
        , unflow
        , take
        , drain
        , slurp

        -- * Map
        , map,          comap

        -- * Dup
        , dup2

        -- * Zip
        , zip
        , zipLeft
        , zipWith
        , zipLeftWith

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

