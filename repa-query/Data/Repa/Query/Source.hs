{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Repa query EDSL.
--
--   A query written with this EDSL expresses an AST/operator graph that
--   compiled into an executable that computes the result of the query.
--
--   The produced AST includes enough type information about the the source
--   data that it can be checked for well-formedness without access to
--   external metadata -- namely the format of tables and the types of
--   their columns.
--
--   This meta-data can either be embedded directly in the query, 
--   or read from a local copy of the table metadata, depending on what
--   operators are used.
--   
module Data.Repa.Query.Source
        ( -- * Types
          Query, Flow, Value

          -- * Query builder
        , Q, query

          -- * Flow operators
          -- | The provided operators are restricted to the set that can be
          --   performed on the fly, without needing to create intermediate
          --   tables.
        , Delim  (..)
        , Field  (..)

          -- ** Sourcing
        , fromFile
        , fromTable
        , fromColumn
        , fromColumns

          -- ** Mapping
        , map, map2, map3, map4, map5

          -- ** Folding
        , fold
        , folds

          -- ** Filtering
        , filter

          -- ** Grouping
        , groups
        , groupsBy

          -- * Scalar operators
          -- ** Arithmetic
        , negate, abs, signum
        , (+),  (-),  (*), (/)
        , (==), (/=)
        , (>),  (>=), (<), (<=)

          -- ** Dates
        , yearOfDate
        , monthOfDate
        , dayOfDate

          -- ** Constructors
        , row0, row1, row2, row3, row4, row5

          -- ** Projections
        , get2_1, get2_2
        , get3_1, get3_2, get3_3
        , get4_1, get4_2, get4_3, get4_4
        , get5_1, get5_2, get5_3, get5_4, get5_5

          -- * Prelude re-exports.
        , ($)
        , return, (>>=), (=<<)
        , P.Int, P.Float, P.Double, P.String, Date32.Date32)
where
import Data.Repa.Query.Source.Builder
import Data.Repa.Query.Source.Literal           ()
import Data.Repa.Query.Source.Operator
import Data.Repa.Query.Source.Projection
import Data.Repa.Query.Source.Scalar
import Data.Repa.Query.Source.Sources
import Data.Repa.Store.Format                   as F
import qualified Data.Repa.Bits.Date32          as Date32
import qualified Prelude                        as P
import Prelude   
 hiding ( ($), return, (>>=), (=<<)
        , map, filter
        , negate, abs, signum
        , (+),  (-), (*), (/)
        , (==), (/=)
        , (>),  (>=), (<), (<=))


---------------------------------------------------------------------------------------------------
-- Re-exports of Prelude things, 
-- so the names are reported from coming from this module in
-- EDSL compile time error messages.
($)     = (\x y -> x P.$ y)
return  = P.return
(>>=)   = (\x y -> x P.>>= y)
(=<<)   = (\x y -> x P.=<< y)


---------------------------------------------------------------------------------------------------
-- | Produce a query, using the given deliminator and field types
--   for the output format.
query   :: F.Delim              -- ^ Output delimitor.
        -> F.Field a            -- ^ Output field types.
        -> Q (Flow a)           -- ^ Output flow.
        -> Q Query

query delim field mkFlow
 = do   flow    <- mkFlow
        return  $ Query delim field flow


