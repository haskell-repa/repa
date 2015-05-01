
module Data.Repa.Query.Job.Spec
        ( -- * Job
          Job           (..)

          -- * Query
        , Query         (..)
        , QueryS        
        , OutputFormat  (..)

          -- * Extract
        , ExtractTarget (..)

          -- * Sieve
        , SieveTarget   (..))
where
import Data.Repa.Query.Graph
import Data.Repa.Store.Format   as Format


---------------------------------------------------------------------------------------------------
-- | Job specification.
data Job
        -- | Read-only query where the output is streamed back
        --   directly to the client.
        = JobQuery
        { jobQuery              :: QueryS 
        , jobOutputFormat       :: OutputFormat }

        -- | Query where the output is written to local storage.
        | JobExtract
        { jobQuery              :: QueryS 
        , jobOutputFormat       :: OutputFormat 
        , jobExtractTarget      :: ExtractTarget }

        -- | Query where the result is a pair of filename and value.
        --   Each value is written to its associated file.
        | JobSieve
        { jobQuery              :: QueryS
        , jobOutputFormat       :: OutputFormat
        , jobSieveTarget        :: SieveTarget }
        deriving Show


---------------------------------------------------------------------------------------------------
-- | A query consisting of an graph, and the name of the output flow.
data Query a nF bV nV
        = Query 
        { -- | Name of output flow in the operator graph.
          queryOutput           :: nF

          -- | Operator graph for the query.
        , queryGraph            :: Graph a nF bV nV
        }

deriving instance (Show a, Show nF, Show bV, Show nV)
        => Show (Query a nF bV nV)


-- | A query using strings for flow and scalar names.
type QueryS
        = Query () String String String


---------------------------------------------------------------------------------------------------
-- | Output format for a query.
data OutputFormat

        -- | Output format is fixed to the given format,
        --   given in the query definition.
        = OutputFormatFixed 
        { -- | How to delimit fields in the output.
          outputFormatDelim     ::  Format.Delim

          -- | Formats for each field in the output.
        , outputFormatFields    :: [Format.FieldBox] }

        -- | Output format is default human readable ascii, determined at
        --   query build time based on the inferred data type of the 
        --   of the result.
        | OutputFormatAsciiBuildTime


deriving instance Show OutputFormat


---------------------------------------------------------------------------------------------------
-- | Target for an extract job.
data ExtractTarget
        -- | Write data to a local file.
        = ExtractTargetFile FilePath
        deriving Show


-- | Target for a sieve job.
data SieveTarget
        -- | Write data to buckets in the given directory.
        = SieveTargetDir  FilePath
        deriving Show

