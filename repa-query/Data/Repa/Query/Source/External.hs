
-- | Re-export of query combinators to be used when building queries defined
--   in external code. We re-export all the basic types and functions the
--   in the Preude the function may need to use, but without IO functions
--   that can access the local file system directly, like readFile / writeFile.
--
module Data.Repa.Query.Source.External
        ( -- * Query types.
        --  Query
          Flow, Value

          -- * Query builder
        , Q
--        , query

          -- * Flow operators
          -- | The provided operators are restricted to the set that can be
          --   performed on the fly, without needing to create intermediate
          --   tables.
        , Delim  (..)
        , Field  (..)

          -- ** Sourcing
        , fromFile
        , fromStore

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
        , row0, row1, row2, row3, row4, row5, row6, row7, row8, row9

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
import Data.Repa.Query.Source.Primitive.Literal         ()
import Data.Repa.Query.Source.Primitive.Operator
import Data.Repa.Query.Source.Primitive.Projection
import Data.Repa.Query.Source.Primitive.Scalar
import Data.Repa.Query.Source.Primitive.Sources
import Data.Repa.Scalar.Date32                          as Date32
import Data.Repa.Store.Format                           as F
import qualified Prelude                                as P
import Prelude   
 hiding ( ($), return, (>>=), (=<<)
        , map, filter
        , negate, abs, signum
        , (+),  (-), (*), (/)
        , (==), (/=)
        , (>),  (>=), (<), (<=))


-- Re-exports of Prelude things, 
-- so the names are reported from coming from this module in
-- EDSL compile time error messages.
($)     = (\x y -> x P.$ y)
return  = P.return
(>>=)   = (\x y -> x P.>>= y)
(=<<)   = (\x y -> x P.=<< y)

