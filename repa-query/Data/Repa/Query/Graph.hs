
module Data.Repa.Query.Graph
        ( Graph         (..)
        , Node          (..)
        , FlowOp        (..)
        , Source        (..)
        , Exp           (..)
        , ScalarOp      (..))
where
import qualified Data.Repa.Convert.Format       as Format


-- | Operator graph for a query.
data Graph a nFlow nValue
        = Graph [Node a nFlow nValue]


-- | A single node in the graph.
data Node a nFlow nValue
        -- | A flow source.
        = NodeSource    (Source a nFlow)

        -- | A flow operator.
        | NodeOp        (FlowOp a nFlow nValue)
        deriving Show


-- | Flow operators.
data FlowOp a nFlow nValue
        -- | Apply a function to every element of a flow.
        = FopMapI
        { fopInput      :: nFlow
        , fopOutput     :: nFlow
        , fopExp        :: Exp a nValue }

        -- | Group sequences of values by the given predicate,
        --   returning lengths of each group.
        | FopGroupsI
        { fopInput      :: nFlow
        , fopOuput      :: nFlow
        , fopExp        :: Exp a nValue }
        deriving Show


-- | Flow sources.
data Source a nFlow
        -- | Source data from a named table.
        = SourceTable
        { -- | Annotation
          sourceAnnot           :: a

          -- | Name of table.
        , sourceTableName       :: String 

        , sourceOutput          :: nFlow }
        deriving Show


-- | Scalar expressions.
data Exp a nValue
        = XVar  a nValue                  -- ^ Scalar Variable
        | XOp   a ScalarOp [Exp a nValue] -- ^ Scalar operator.
        | XLit  a Lit                     -- ^ Scalar literal.
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


