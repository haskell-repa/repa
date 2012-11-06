
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

        -- * Construction
        , generate
        , replicate
        , replicatesUnboxed
        , replicatesDirect
        , enumFromN

        -- * Mapping
        , map
        , zip,          zipLeft
        , zipWith,      zipLeftWith

        -- * Packing
        , pack
        , packByTag
        , filter

        -- * Projection
        , gather

        -- * Reduction
        , foldl
        , folds
        , sums)

where
import Data.Array.Repa.Flow.Seq.Base
import Data.Array.Repa.Flow.Seq.Generate
import Data.Array.Repa.Flow.Seq.Map
import Data.Array.Repa.Flow.Seq.Pack
import Data.Array.Repa.Flow.Seq.Project
import Data.Array.Repa.Flow.Seq.Fold
import Prelude  hiding (map, zip, zipWith, foldl, filter, replicate, take)

