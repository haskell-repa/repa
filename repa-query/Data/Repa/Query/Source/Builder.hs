module Data.Repa.Query.Source.Builder
        ( Flow (..)
        , makeFlow, takeFlow

        , Value (..)
        , Q
        , runQ
        , newFlow
        , addNode)
where
import Control.Monad.State.Strict
import qualified Data.Repa.Query.Graph  as G


---------------------------------------------------------------------------------------------------
-- | Flows of the given element type.
--
--   Internally, this is a wrapper around a flow variable name.
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
-- | Scalar values of the given type.
--
--   Internally, this is a wrapper around an expression that
--   computes the value.
data Value a
        = Value   (G.Exp () () Int)
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Query building monad.
type Q a
        = State S a


-- | Run a query builder.
-- 
--   The operator graph in the result query uses strings for flow variables
--   and debruijn indices for value variables.
--
runQ :: Q (Flow a) -> G.Query () String () Int
runQ f
 = let (Flow x, s')  = runState f
                $ S { sNodes        = []
                    , sGenFlow      = 0
                    , sGenScalar    = 0 }
   in  G.Query x (G.Graph (sNodes s'))


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
        modify  $ \s -> s { sGenFlow = ix + 1}
        return  $ makeFlow $ "f" ++ show ix


-- | Add a new node to the graph
addNode :: G.Node () String () Int -> Q ()
addNode n
 = modify $ \s -> s { sNodes = sNodes s ++ [n] }

