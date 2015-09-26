
module Data.Repa.Array.Auto
        ( Array
        , Elem, Build

        -- * Basics
        , index
        , (!)
        , length
        , first, last
        , head, init, tail

        -- * Construction
        , empty
        , singleton
        , generateMaybeS,  mapMaybeS
        , generateEitherS, mapEitherS

        -- * Conversion
        , fromList
        , fromLists
        , fromListss

        , toList
        , toLists
        , toListss

        -- * Operators

        -- ** Replicating
        , replicates

        -- ** Mapping
        , map
        , map2
        , mapElems

        -- ** Folding
        , foldl
        , sum,  product
        , mean, std
        , correlate
        , folds
        , foldsWith

        -- ** Filtering
        , filter
        , slices
        , trims
        , trimEnds
        , trimStarts

        -- ** Zipping
        , zip
        , unzip

        -- ** Sloshing
        , reverse
        , concat
        , concats
        , concatWith
        , unlines
        , intercalate
        , ragspose3

        -- ** Slicing
        , slice

        -- ** Inserting
        , insert

        -- ** Searching
        , findIndex

        -- ** Merging
        , merge
        , mergeMaybe

        -- ** Compacting
        , compact
        , compactIn

        -- ** Processing
        , process

        -- ** Grouping
        , groups
        , groupsWith

        -- ** Splitting
        , segment
        , segmentOn
        , dice
        , diceSep

        -- ** Products

        -- *** Generic
        , prod
        , unprod

        -- *** Pattern synonyms
        -- | These patterns patterns work on arrays of arbitrary element type.
        --   
        --   (Haddock 2.16.0 has a bug where the type signatures come out monomorphic)
        --
        , pattern Prod2
        , pattern Prod3
        , pattern Prod4
        , pattern Prod5
        , pattern Prod6
        , pattern Prod7
        , pattern Prod8
        , pattern Prod9)
where
import Data.Repa.Array.Auto.Base
import Data.Repa.Array.Auto.Operator
import Data.Repa.Array.Material.Auto.InstProduct
import Prelude 
       hiding   ( map,  length, reverse, filter, concat, unlines, foldl
                , sum,  product
                , zip,  unzip
                , head, init, tail, last)



