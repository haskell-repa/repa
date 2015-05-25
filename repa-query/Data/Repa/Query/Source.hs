
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
          Job, Query, Flow, Value, Q

          -- * Job Builders
          -- ** Query
        , query

          -- ** Extract
        , extract,      ExtractTarget (..)

          -- ** Sieve
        , sieve,        SieveTarget   (..)

          -- ** Targets
        , TargetFile(..)
        , TargetDir (..)

          -- * Flow operators
          -- | The provided operators are restricted to the set that can be
          --   performed on the fly, without needing to create intermediate
          --   tables.

          -- ** Sourcing
        , fromFile
        , fromStore
        , fromStoreColumns

          -- ** Mapping
        , map, map2, map3, map4, map5, map6, map7

          -- ** Zipping
        , zip, zip2, zip3, zip4, zip5, zip6, zip7

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
        , stringOfDate
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

          -- * Output formats
        , F.Delim  (..)
        , F.Field  (..))
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
import qualified Data.Repa.Store.Format                 as F
import Data.Repa.Scalar.Product
import Prelude 
 hiding ( map
        , zip, zip3
        , filter)


-------------------------------------------------------------------------------
-- | Query using the default ASCII output format.
query   :: Q (Flow a) 
        -> Config 
        -> IO (Either String [Job])

query mkFlow config
 =  makeQuery config mkFlow $ \q
 -> return $ Right [JobQuery q J.OutputFormatAsciiBuildTime]


-------------------------------------------------------------------------------
-- | Extract data to the given target.
extract :: ExtractTarget 
        -> Q (Flow a)
        -> Config
        -> IO (Either String [Job])

extract target mkFlow config
 =  makeQuery config mkFlow $ \q
 -> return $ Right [JobExtract q J.OutputFormatAsciiBuildTime target]


-------------------------------------------------------------------------------
-- | Sieve rows to named buckets in a directory.
sieve   :: SieveTarget
        -> Q (Flow (String :*: a))
        -> Config
        -> IO (Either String [Job])

sieve target mkFlow config
 =  makeQuery config mkFlow $ \q
 -> return $ Right [JobSieve q J.OutputFormatAsciiBuildTime target]


-------------------------------------------------------------------------------
-- | Evaluate a flow builder, 
--   and convert it to a query graph.
makeQuery 
        :: Config
        -> Q (Flow a)
        -> (QueryS -> IO (Either String b))
        -> IO (Either String b)

makeQuery config mkFlow k
 = do
        (state', eFlow) 
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
                            $ J.Query vFlow (G.Graph (sNodes state'))
                k q


-------------------------------------------------------------------------------
class TargetFile t where
 -- | Lift a file path to a target.
 toFile :: FilePath -> t

instance TargetFile ExtractTarget where
 toFile  path   = ExtractTargetFile path


class TargetDir t where
 -- | Lift a directory name to a target.
 toDir  :: FilePath -> t

instance TargetDir  SieveTarget where
 toDir   path   = SieveTargetDir path

