
module Data.Repa.Array.Auto
        ( Array
        , Elem, Build

        -- * Basics
        , index
        , (!)
        , length

        -- * Conversion
        , fromList
        , fromLists
        , fromListss

        , toList
        , toLists
        , toListss

        -- * Operators

        -- ** Mapping
        , map
        , map2
        , mapElems

        -- ** Folding
        , foldl
        , sum,  prod
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

        -- ** Sloshing
        , reverse
        , concat
        , concats
        , concatWith
        , unlines
        , intercalate
        , ragspose3

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

        -- ** Grouping
        , groups
        , groupsWith

        -- ** Splitting
        , segment
        , segmentOn
        , dice
        , diceSep

        -- ** Conversion
        -- *** Foreign arrays
        , fromForeign, toForeign

        -- *** Int Conversion
        , readIntFromOffset
        , readIntFromOffset# )        
where
import Data.Repa.Array.Auto.Base
import Data.Repa.Array.Auto.Operator
import Prelude 
       hiding (map, length, reverse, filter, concat, unlines, foldl, sum)

