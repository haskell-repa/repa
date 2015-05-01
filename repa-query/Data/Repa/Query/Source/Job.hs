
module Data.Repa.Query.Source.Job
        ( Job           (..)
        , Query         (..)
        , QueryS        
        , OutputFormat  (..))
where
import Data.Repa.Query.Graph
import Data.Repa.Store.Format   as Format

---------------------------------------------------------------------------------------------------
data Job
        -- | Read-only query that produces a flow of data
        --   following the embedded output format.
        = JobQuery
        { jobQuery              :: QueryS 
        , jobOutputFormat       :: OutputFormat }

        | JobExtract
        { jobExtract            :: QueryS 
        , jobOutputFormat       :: OutputFormat 
        , jobExtractTarget      :: ExtractTarget }


---------------------------------------------------------------------------------------------------
-- | A query consisting of an graph, and the name of the output flow.
data Query a nF bV nV
        = Query 
        { -- | Output format for data.
          queryOutputFormat     :: OutputFormat

          -- | Name of output flow in the operator graph.
        , queryOutput           :: nF

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
-- | Target for an extract.
data ExtractTarget
        -- | Write data to a local file.
        = ExtractTargetFile FilePath


