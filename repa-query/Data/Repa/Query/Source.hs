
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
        ( -- * Query Types
          Query, Flow, Value

          -- * Query builder
        , Q, query
--        , queryAs

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
        , get5_1, get5_2, get5_3, get5_4, get5_5)
where
import Data.Repa.Query.Job.Spec                         as J
import Data.Repa.Query.Graph                            as G
import Data.Repa.Query.Transform.Namify                 as N
import Data.Repa.Query.Source.Builder                   as S
import Data.Repa.Query.Source.Primitive.Literal         ()
import Data.Repa.Query.Source.Primitive.Operator
import Data.Repa.Query.Source.Primitive.Projection
import Data.Repa.Query.Source.Primitive.Scalar
import Data.Repa.Query.Source.Primitive.Sources
import Data.Repa.Store.Format                           as F
import Prelude 
        hiding (map, filter)


---------------------------------------------------------------------------------------------------
-- | Produce a query using the default ASCII output format.
query   :: Q (Flow a) 
        -> Config 
        -> IO (Either String [Job])

query mkFlow config
 = do   (state', eFlow) 
                <- evalQ mkFlow 
                $  State { sConfig      = config
                         , sNodes       = []
                         , sGenFlow     = 0
                         , sGenScalar   = 0 }

        case eFlow of
         Left  err
          -> return $ Left err

         Right (Flow vFlow)
          -> do let Just q  = N.namify N.mkNamifierStrings
                            $ J.Query
                                J.OutputFormatAsciiBuildTime
                                vFlow
                                (G.Graph (sNodes state'))

                return $ Right [JobQuery q J.OutputFormatAsciiBuildTime]


{-
-- | Produce a query using the given deliminator and row format.
queryAs :: F.Delim -> F.Field a
        -> Q (Flow a)
        -> Q Query

queryAs delim field mkFlow
 = do   flow    <- mkFlow
        return  $ QueryFixed delim field flow
-}









