
module Data.Repa.Query.EDSL
        ( Q
        , runQ
        , Flow  (..)
        , Value (..)

        -- * Flow operators
        , source
        , map
        , groups

        -- * Scalar operators
        , (+))
where
import Control.Monad.State.Strict
import Prelude          hiding (map, (+))
import qualified Data.Repa.Query.Graph  as G
import qualified Prelude                as P


---------------------------------------------------------------------------------------------------
-- | Type of flows. 
data Flow a
        = Flow     String
        deriving Show


-- | Wrap a flow name with its phantom type.
makeFlow :: String -> Flow a
makeFlow name = Flow name


-- | Unwrap a flow name.
takeFlow :: Flow a -> String
takeFlow (Flow name) = name


---------------------------------------------------------------------------------------------------
-- | Type of scalar values.
data Value a
        = Value   (G.Exp () () Int)
        deriving Show



---------------------------------------------------------------------------------------------------
-- | Query building monad.
type Q a
        = State S a


-- | Run a query building monad.
runQ :: Q a -> (a, S)
runQ f
 = runState f
 $ S      { sNodes        = []
          , sGenFlow      = 0
          , sGenScalar    = 0 }


-- | State used when building the operator graph.
data S  = S
        { -- | We strip the type information from the nodes so we can put
          --   them all in the graph. Flows are named with strings, while
          --   scalars are named with debruijn indices.
          sNodes        :: [G.Node () String () Int]
        , sGenFlow      :: Int
        , sGenScalar    :: Int }
        deriving Show
        

-- | Allocate a new node name.
newFlow :: Q (Flow a)
newFlow 
 = do   ix      <- gets sGenFlow
        modify  $ \s -> s { sGenFlow = ix P.+ 1}
        return  $ makeFlow $ "f" ++ show ix


-- | Add a new node to the graph
addNode :: G.Node () String () Int -> Q ()
addNode n
 = modify $ \s -> s { sNodes = sNodes s ++ [n] }


---------------------------------------------------------------------------------------------------
-- | Source a table.
source :: String -> Q (Flow a)
source table
 = do   fOut    <- newFlow
        addNode $ G.NodeSource (G.SourceTable () table $ takeFlow fOut)
        return  fOut


-- | Apply a scalar function to every element of a flow.
map :: (Value a -> Value a) -> Flow a -> Q (Flow a)
map fun fIn
 = do   fOut    <- newFlow

        addNode $ G.NodeOp 
         $ G.FopMapI 
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam $ let Value x  = fun $ Value $ xVar 0
                        in  x)

        return fOut


-- | Scan throw a flow to find runs of consecutive values,
--   yielding the value and the length of each run.
groups :: Flow a -> Q (Flow (a, Int))
groups fIn
 = do   fOut    <- newFlow

        addNode $ G.NodeOp  
         $ G.FopGroupsI
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam $ xLam $ xOp G.SopEq [xVar 0, xVar 1])

        return fOut


---------------------------------------------------------------------------------------------------
-- | Scalar addition.
(+) :: Value a -> Value a -> Value a
(+) (Value x1) (Value x2)
        = Value $ xOp G.SopAdd [x1, x2]



---------------------------------------------------------------------------------------------------
xVar i          = G.XVar () i
xLam x          = G.XLam () () x
xOp s args      = G.XOp  () s args


