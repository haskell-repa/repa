
-- | Sequential flows provide an incremental version of array fusion that
--   allows the computation to be suspended and resumed at a later time.
module Data.Array.Repa.Flow.Seq
        ( FD, FS
        , Flow (..)
        , Step1(..)
        , Step8(..)
        
        -- * Conversion
        , flow
        , unflow
        , take
        , drain
        , slurp

        -- * Construction
        , generate
        , replicate
        , replicatesDirect
        , enumFromN
        , appends

        -- * Map
        , map
        , zip,          zipLeft
        , zipWith,      zipLeftWith

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
import Data.Array.Repa.Flow.Seq.Flow
import Data.Array.Repa.Flow.Seq.Operator.Generate
import Data.Array.Repa.Flow.Seq.Operator.Append
import Data.Array.Repa.Flow.Seq.Operator.Map
import Data.Array.Repa.Flow.Seq.Operator.Pack
import Data.Array.Repa.Flow.Seq.Operator.Combine
import Data.Array.Repa.Flow.Seq.Operator.Project
import Data.Array.Repa.Flow.Seq.Operator.Fold
import Prelude  hiding (map, zip, zipWith, unzip, foldl, filter, replicate, take)

