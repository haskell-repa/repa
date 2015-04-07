
module Data.Repa.Query.EDSL
        ( Q
        , Exp
        , runQ
        , newFlow
--        , newNameScalar
        , addNode)
where
import Control.Monad.State.Strict
import Control.Monad.State.Class
import Prelude          hiding (map)
import qualified Data.Repa.Query.Graph  as G


type Exp 
        = G.Exp () Value

data Flow
        = Flow     String
        deriving Show

data Value
        = From     Flow
        deriving Show


-- | Query building monad.
type Q a
        = State S a


runQ :: Q a -> (a, S)
runQ f
 = runState f
 $ S      { sNodes        = []
          , sGenFlow      = 0
          , sGenScalar    = 0 }

data S  = S
        { sNodes        :: [G.Node () Flow Value]
        , sGenFlow      :: Int
        , sGenScalar    :: Int }
        deriving Show
        

-- | Allocate a new node name.
newFlow :: Q Flow
newFlow 
 = do   ix      <- gets sGenFlow
        modify  $ \s -> s { sGenFlow = ix + 1}
        return  $ Flow $ "f" ++ show ix

{-
-- | Allocate a new node name.
newNameScalar :: Exp String     
newNameScalar 
 = do   ix      <- gets sGenScalar
        modify  $ \s -> s { sGenScalar = ix + 1}
        return  $ "x" ++ show ix
-}

-- | Add a new node to the graph
addNode :: G.Node () Flow Value -> Q ()
addNode n
 = modify $ \s -> s { sNodes = sNodes s ++ [n] }


add :: Exp -> Exp -> Exp
add x1 x2
 = G.XOp () G.SopAdd [x1, x2]

---------------------------------------------------------------------------------------------------
-- | Source a table.
source :: String -> Q Flow
source table
 = do   fOut    <- newFlow
        addNode $ G.NodeSource (G.SourceTable () table fOut)
        return fOut


map :: (Value -> Value) -> Flow -> Q Flow
map fun fIn
 = do   fOut    <- newFlow
        addNode $ G.NodeOp (G.FopMapI fIn fOut (G.XVar () (fun $ From fIn)))
        return fOut


{-}

do      x <- source "things"
        y <- map (lam (\x -> add x 1)) x
        return y

-}
