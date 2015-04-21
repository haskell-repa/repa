module Data.Repa.Query.Source.Builder
        ( -- * Queries
          Query (..)

          -- * Flows
        , Flow  (..)
        , makeFlow, takeFlow

          -- * Values
        , Value (..)

          -- * Query builder monad.
        , Q, Config (..)
        , runQ
        , newFlow
        , addNode)
where
import qualified Control.Monad.Trans.State.Strict       as S
import qualified Data.Repa.Query.Format                 as F
import qualified Data.Repa.Query.Graph                  as G
import qualified Data.Repa.Query.Transform.Namify       as N


---------------------------------------------------------------------------------------------------
-- | A complete query.
data Query
        = forall a. Query   
        { queryOutDelim :: F.Delim
        , queryOutField :: F.Field a
        , queryOutFlow  :: Flow    a }

deriving instance Show Query


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
        = S.StateT State IO a


-- | Query builder config.
data Config
        = Config
        { -- | Path to data root, containing meta-data for used tables.
          configRoot    :: FilePath }
        deriving Show


-- | Run a query builder.
--   
--   The provided config contains the path to the meta data needed by
--   operators like `sourceTable`.
--   
runQ    :: Config               -- ^ Query builder config.
        -> Q  Query             -- ^ Computation to produce query AST.
        -> IO (G.Query () String String String)

runQ config mkQuery
 = do   
        -- Run the query builder to get the AST / operator graph.
        (Query delim field (Flow vFlow), state')
                <- S.runStateT mkQuery
                $  State { sConfig      = config
                         , sNodes       = []
                         , sGenFlow     = 0
                         , sGenScalar   = 0 }

        -- The nodes added to the state use debruijn indices for variables,
        -- but we'll convert them to named variables while we're here.
        --
        -- This match should always succeed because the namifier only returns
        -- Nothing when there are out of scope variables. However, the only 
        -- way we can construct a (Q (Flow a)) is via the EDSL code, which
        -- doesn't provide a way of producing expressions with free indices.
        --
        let Just q  
                = N.namify N.mkNamifierStrings 
                $ G.Query vFlow delim 
                        (F.flattens field)
                        (G.Graph (sNodes state'))
        return q
 

 ---------------------------------------------------------------------------------------------------
-- | State used when building the operator graph.
data State  
        = State
        { -- | Query builder config
          sConfig       :: Config

          -- | We strip the type information from the nodes so we can put
          --   them all in the graph. Flows are named with strings, while
          --   scalars are named with debruijn indices.
        , sNodes        :: [G.Node () String () Int]

          -- | Counter to generate fresh flow variable names.
        , sGenFlow      :: Int

          -- | Counter to generate fresh scalar variable names.
        , sGenScalar    :: Int }
        deriving Show
        

-- | Allocate a new node name.
newFlow :: Q (Flow a)
newFlow 
 = do   ix        <- S.gets sGenFlow
        S.modify  $ \s -> s { sGenFlow = ix + 1}
        return    $ makeFlow $ "f" ++ show ix


-- | Add a new node to the graph
addNode :: G.Node () String () Int -> Q ()
addNode n
 = S.modify $ \s -> s { sNodes = sNodes s ++ [n] }


