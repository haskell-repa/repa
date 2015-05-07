
module Data.Repa.Query.Graph.Graph      
        ( -- * Graphs
          Graph         (..)
        , Node          (..)

          -- * Flow sources
        , Source        (..)

          -- * Flow operators
        , FlowOp        (..)

          -- * Scalar expressions
        , Exp           (..)
        , Val           (..)
        , Lit           (..)
        , ScalarOp      (..))
where
import Data.Repa.Query.Graph.Exp
import qualified Data.Repa.Store.Format as Format


---------------------------------------------------------------------------------------------------
-- | Operator graph for a query.
data Graph a nF bV nV
        = Graph [Node a nF bV nV]
        deriving Show


---------------------------------------------------------------------------------------------------
-- | A single node in the graph.
data Node a nF bV nV
        -- | A flow source.
        = NodeSource    (Source a nF)

        -- | A flow operator.
        | NodeOp        (FlowOp a nF bV nV)
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Flow sources.
data Source a nF
        -- | Source complete rows from a flat file.
        = SourceFile
        { sourceAnnot           :: a                    -- ^ Annotation.
        , sourceFilePath        :: FilePath             -- ^ Path to file.
        , sourceDelim           :: Format.Delim         -- ^ Delimitor for elements.
        , sourceFields          :: [Format.FieldBox]    -- ^ Format of fields.
        , sourceOutput          :: nF                   -- ^ Output flow.
        }

        -- TODO: merge the table sourcing nodes into just the single
        --       SourceTableColumns then fix the column ordering.
        -- | Source complete rows from a table.
        | SourceTable
        { sourceAnnot           :: a                    -- ^ Annotation.
        , sourceFilePath        :: FilePath             -- ^ Path to table.
        , sourceDelim           :: Format.Delim         -- ^ Delimitor for elements.
        , sourceFields          :: [Format.FieldBox]    -- ^ Format of fields.
        , sourceOutput          :: nF                   -- ^ Output flow.
        }

        -- | Source a single column from a table.
        | SourceTableColumn
        { sourceAnnot           :: a                    -- ^ Annotation.
        , sourceFilePath        :: FilePath             -- ^ Path to table.
        , sourceDelim           :: Format.Delim         -- ^ Delimitor for elements.
        , sourceFields          :: [Format.FieldBox]    -- ^ Format of all fields in table.
        , sourceIxColumn        :: (String, Int)        -- ^ Index and name of desired column.
        , sourceOutput          :: nF }                 -- ^ Output flow.

        -- | Source a subset of columns from a table.
        --   The type of the resulting elements is list-like, eg (col1 :*: col2 :*: ())
        --
        | SourceTableColumns
        { sourceAnnot           :: a                    -- ^ Annotation.
        , sourceFilePath        :: FilePath             -- ^ Path to table.
        , sourceDelim           :: Format.Delim         -- ^ Delimitor for elements.
        , sourceFields          :: [Format.FieldBox]    -- ^ Format of all fields in table.
        , sourceIxColumns       :: [(String, Int)]      -- ^ Index and name of desired columns.
        , sourceOutput          :: nF                   -- ^ Output flow.
        }

        -- | Source a single column in a column family.
        | SourceFamilyColumns
        { sourceAnnot           :: a                    -- ^ Annotation.
        , sourcePathFamily      :: FilePath             -- ^ Path  to column family.
        , sourcePathColumns     :: [FilePath]           -- ^ Paths to wanted columns.
        , sourceFormatKey       :: Format.FieldBox      -- ^ Format of family key.
        , sourceFormatColumns   :: [Format.FieldBox]    -- ^ Format of wanted columns.
        , sourceOutput          :: nF }                 -- ^ Output flow.


deriving instance (Show a, Show nF) 
        => Show (Source a nF)


---------------------------------------------------------------------------------------------------
-- | Flow operators.
data FlowOp a nF bV nV
        -- | Combine corresponding elements of some flows with a function.
        --   INVARIANT: there must be at least one element in the inputs list.
        = FopMapI
        { fopInputs             :: [nF]                 -- ^ Input flow.
        , fopOutput             :: nF                   -- ^ Output flow.
        , fopFun                :: Exp a bV nV          -- ^ Worker function.
        }

        -- | Keep only the elements that match the given predicate.
        | FopFilterI
        { fopInput              :: nF                   -- ^ Input flow.
        , fopOutput             :: nF                   -- ^ Output flow.
        , fopFun                :: Exp a bV nV          -- ^ Filter predicate.
        }

        -- | Fold all the elements of a flow, 
        --   yielding a new flow of a single result element.
        | FopFoldI      
        { fopInput              :: nF                   -- ^ Input flow.
        , fopOutput             :: nF                   -- ^ Output flow.
        , fopFun                :: Exp a bV nV          -- ^ Worker function.
        , fopNeutral            :: Exp a bV nV          -- ^ Neutral value of worker.
        }       

        -- | Segmented fold of the elements of a flow.
        | FopFoldsI     
        { fopInputLens          :: nF                   -- ^ Input flow for lengths.
        , fopInputElems         :: nF                   -- ^ Input flow for elements.
        , fopOutput             :: nF                   -- ^ Output flow.
        , fopFun                :: Exp a bV nV          -- ^ Worker function.
        , fopNeutral            :: Exp a bV nV          -- ^ Neutral value of worker.
        }

        -- | Group sequences of values by the given predicate,
        --   returning lengths of each group.
        | FopGroupsI
        { fopInput              :: nF                   -- ^ Input flow.
        , fopOuput              :: nF                   -- ^ Output flow.
        , fopFun                :: Exp a bV nV          -- ^ Comparison function for groups.
        }
        deriving Show

