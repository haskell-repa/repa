
module Data.Repa.Query.Graph
        ( -- * Queries
          Query         (..)

          -- * Graphs
        , Graph         (..)
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
import Data.Repa.Query.Exp
import qualified Data.Repa.Query.Format as Format


-- | A query consisting of an graph, and the name of the output flow.
data Query a nF bV nV
        = Query 
        { -- | Name of output flow.
          queryOutput           :: nF 

          -- | How to delimit elements in output.
        , queryResultDelim      :: Format.Delim

          -- | Format of fields in output.
        , queryResultFields     :: [Format.FieldBox] 

          -- | Query operator graph.
        , queryGraph            :: Graph a nF bV nV }

deriving instance (Show a, Show nF, Show bV, Show nV)
        => Show (Query a nF bV nV)


-- | Operator graph for a query.
data Graph a nF bV nV
        = Graph [Node a nF bV nV]
        deriving Show


-- | A single node in the graph.
data Node a nF bV nV
        -- | A flow source.
        = NodeSource    (Source a nF)

        -- | A flow operator.
        | NodeOp        (FlowOp a nF bV nV)
        deriving Show


-- | Flow sources.
data Source a nF
        -- | Source data from a flat file.
        =  SourceFile
        { -- | Annotation
          sourceAnnot           :: a

          -- | Name of table.
        , sourceFilePath        :: FilePath

          -- | Delimitor for elements.
        , sourceDelim           :: Format.Delim

          -- | Format of fields.
        , sourceFields          :: [Format.FieldBox]

          -- | Output flow.
        , sourceOutput          :: nF }

deriving instance (Show a, Show nF) 
        => Show (Source a nF)


-- | Flow operators.
data FlowOp a nF bV nV
        -- | Apply a function to every element of a flow.
        = FopMapI
        { fopInput      :: nF
        , fopOutput     :: nF
        , fopFun        :: Exp a bV nV }

        -- | Keep only the elements that match the given predicate.
        | FopFilterI
        { fopInput      :: nF
        , fopOutput     :: nF
        , fopFun        :: Exp a bV nV }

        -- | Fold all the elements of a flow, 
        --   yielding a new flow of a single result element.
        | FopFoldI      
        { fopInput      :: nF
        , fopOutput     :: nF
        , fopFun        :: Exp a bV nV 
        , fopNeutral    :: Exp a bV nV }

        -- | Segmented fold of the elements of a flow.
        | FopFoldsI     
        { fopInputLens  :: nF
        , fopInputElems :: nF
        , fopOutput     :: nF
        , fopFun        :: Exp a bV nV
        , fopNeutral    :: Exp a bV nV }

        -- | Group sequences of values by the given predicate,
        --   returning lengths of each group.
        | FopGroupsI
        { fopInput      :: nF
        , fopOuput      :: nF
        , fopFun        :: Exp a bV nV }
        deriving Show

