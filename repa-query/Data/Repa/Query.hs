
module Data.Repa.Query
        ( -- * Types
          Q.Q, Q.Query, Q.Flow, Q.Value

          -- * Query construction
        , Q.query

          -- * Flow operators
          -- | The provided operators are restricted to the set that can be
          --   performed on the fly, without needing to create intermediate
          --   tables.
        , Q.Delim  (..)
        , Q.Field  (..)

          -- ** Sourcing
        , Q.fromFile
        , Q.fromTable
        , Q.fromColumn
        , Q.fromColumns

          -- ** Mapping
        , Q.map, Q.map2, Q.map3, Q.map4, Q.map5

          -- ** Folding
        , Q.fold
        , Q.folds

          -- ** Filtering
        , Q.filter

          -- ** Grouping
        , Q.groups
        , Q.groupsBy

          -- ** Dates
        , Q.yearOfDate
        , Q.monthOfDate
        , Q.dayOfDate

          -- ** Constructors
        , Q.row0, Q.row1, Q.row2, Q.row3, Q.row4
        , Q.row5, Q.row6, Q.row7, Q.row8, Q.row9

          -- ** Projections
        , Q.get2_1, Q.get2_2
        , Q.get3_1, Q.get3_2, Q.get3_3
        , Q.get4_1, Q.get4_2, Q.get4_3, Q.get4_4
        , Q.get5_1, Q.get5_2, Q.get5_3, Q.get5_4, Q.get5_5)
where
import Data.Repa.Query.Source           as Q
