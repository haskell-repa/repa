
module Data.Repa.Query.Graph
        ( Graph         (..)
        , Node          (..)
        , FlowOp        (..)
        , Source        (..)
        , Exp           (..)
        , ScalarOp      (..)
        , Lit           (..))
where


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


-- | Flow operators.
data FlowOp a nF bV nV
        -- | Apply a function to every element of a flow.
        = FopMapI
        { fopInput      :: nF
        , fopOutput     :: nF
        , fopExp        :: Exp a bV nV }

        -- | Keep only the elements that match the given predicate.
        | FopFilterI
        { fopInput      :: nF
        , fopOutput     :: nF
        , fopExp        :: Exp a bV nV }

        -- | Fold all the elements of a flow, 
        --   yielding a new flow of a single result element.
        | FopFoldI      
        { fopInput      :: nF
        , fopOutput     :: nF
        , fopExp        :: Exp a bV nV 
        , fopNeutral    :: Exp a bV nV }

        -- | Segmented fold of the elements of a flow.
        | FopFoldsI     
        { fopInputLens  :: nF
        , fopInputElems :: nF
        , fopOutput     :: nF
        , fopExp        :: Exp a bV nV
        , fopNeutral    :: Exp a bV nV }

        -- | Group sequences of values by the given predicate,
        --   returning lengths of each group.
        | FopGroupsI
        { fopInput      :: nF
        , fopOuput      :: nF
        , fopExp        :: Exp a bV nV }
        deriving Show


-- | Flow sources.
data Source a nF
        -- | Source data from a named table.
        = SourceTable
        { -- | Annotation
          sourceAnnot           :: a

          -- | Name of table.
        , sourceTableName       :: String 

        , sourceOutput          :: nF }
        deriving Show


-- | Scalar expressions.
data Exp a bV nV
        = XLit  a Lit                    -- ^ Scalar literal.
        | XVar  a nV                     -- ^ Scalar variable.
        | XLam  a bV (Exp a bV nV)       -- ^ Scalar binder.
        | XOp   a ScalarOp [Exp a bV nV] -- ^ Scalar operator.
        deriving (Eq, Show)


-- | Scalar operators.
data ScalarOp
        = SopNeg                        -- ^ Negation.
        | SopAdd                        -- ^ Addition.
        | SopSub                        -- ^ Subtraction.
        | SopMul                        -- ^ Multiplication.
        | SopDiv                        -- ^ Division.
        | SopEq                         -- ^ Equality.
        | SopNeq                        -- ^ Negated equality.
        | SopGt                         -- ^ Greater-than.
        | SopGe                         -- ^ Greater-than or equal.
        | SopLt                         -- ^ Less-than.
        | SopLe                         -- ^ Less-than or equal.
        deriving (Eq, Show)


-- | Literals.
data Lit
        = LitInt        Integer         -- ^ Literal integer.
        | LitFloat      Double          -- ^ Literal float.
        | LitString     String          -- ^ Literal string.
        deriving (Eq, Show)


