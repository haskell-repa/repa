
-- | TODO: 
--      Add unflowLog to emit the report to the GHC eventlog.
--      Use the report to decide whether we should bother sparking
--      off a parallel computation. Via inlining we can probably
--      convert a report to a function that just loads the state and
--      computes the expected cost directly.
--
module Data.Array.Repa.Flow.Seq.Report
        (Report(..))
where

-- | A flow report describes the current state of a flow.
data Report
        -- Conversion
        = Flow  { flowLength            :: Int
                , flowPosition          :: Int  }

        -- Construction
        | Generate
                { generateLength        :: Int
                , generatePosition      :: Int  }

        | Replicate
                { replicateLength       :: Int  }

        | Replicates    
                { replicatesLength      :: Int  }

        | EnumFromN
                { enumFromNLength       :: Int
                , enumFromNPosition     :: Int }

        -- Combinators
        | Map   { mapSource             :: Report }

        | Zip   { zipSource1            :: Report
                , zipSource2            :: Report }

        | ZipLeft
                { zipLeftSource         :: Report }

        | ZipWith
                { zipWithSource1        :: Report 
                , zipWithSource2        :: Report }

        | Pack
                { packSource            :: Report }

        | PackByTag
                { packByTagSource       :: Report }

        | Filter
                { filterSource          :: Report }

        | Gather
                { gatherSource          :: Report }

        -- Reduction
        | Folds { foldsLengths          :: Report 
                , foldsElems            :: Report }

        | Sums  { sumsLengths           :: Report
                , sumsElems             :: Report }
        deriving Show
