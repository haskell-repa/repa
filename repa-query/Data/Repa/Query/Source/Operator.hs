
module Data.Repa.Query.Source.Operator
        ( map
        , fold, folds
        , filter
        , groups, groupsBy)
where
import Control.Monad.State.Strict
import Data.Repa.Query.Source.Builder
import Data.Repa.Product
import Data.Int
import Data.Repa.Query.Source.Compounds         as C
import qualified Data.Repa.Query.Graph          as G
import Prelude   
 hiding ( map, filter
        , negate, abs, signum)


-- | Apply a scalar function to every element of a flow.
map :: (Value a -> Value b) -> Flow a -> Q (Flow b)
map fun fIn
 = do   fOut    <- newFlow

        addNode $ G.NodeOp 
         $ G.FopMapI 
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam $ let Value x  = fun $ Value $ xVar 0
                        in  x)

        return fOut


-- | Combine all elements of a flow using the given operator and neutral value.
--   The result is returned in a new flow of a single element.
--
--   To support parallel evaluation, the neutral value must be both a left and
--   right unit of the combining operator, otherwise the result is undefined.
--   For example use, (+) as the operator and 0 as the neutral value to perform
--   a sum.
--
-- @ 
--   x + 0  = x  (0 is left-unit  of (+))
--   0 + x  = x  (0 is right-unit of (+))
-- @
--
fold    :: (Value a -> Value a -> Value a)
        -> Value a 
        -> Flow a -> Q (Flow a)

fold fun (Value z) fIn
 = do   fOut    <- newFlow

        addNode $ G.NodeOp
         $ G.FopFoldI
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam $ xLam $ let Value x = fun (Value $ xVar 0) (Value $ xVar 1)
                               in  x)
                z

        return fOut


-- | Segmented fold. Like `fold`, but combine consecutive runs of elements. 
--   The length of each run is taken from a second flow.
folds   :: (Value a -> Value a -> Value a)
        -> Value a
        -> Flow    (n :*: Int :*: ()) 
        -> Flow a
        -> Q (Flow (n :*: a   :*: ()))

folds fun (Value z) fLens fElems
 = do   fOut    <- newFlow

        addNode $ G.NodeOp
         $ G.FopFoldsI
                (takeFlow fLens)
                (takeFlow fElems)
                (takeFlow fOut)
                (xLam $ xLam $ let Value x = fun (Value $ xVar 0) (Value $ xVar 1)
                               in  x)
                z

        return fOut


-- | Keep only the elements that match the given predicate.
filter  :: (Value a -> Value Bool)
        -> Flow a -> Q (Flow a)

filter fun fIn
 = do   fOut    <- newFlow

        addNode $ G.NodeOp
         $ G.FopFilterI
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam $ let Value x  = fun $ Value $ xVar 0
                        in  x)

        return fOut


-- | Scan through a flow to find runs of consecutive values,
--   yielding the value and the length of each run.
groups :: Flow a -> Q (Flow (a :*: Int :*: ()))
groups fIn
 = do   fOut    <- newFlow

        addNode $ G.NodeOp  
         $ G.FopGroupsI
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam $ xLam $ xOp G.SopEq [xVar 0, xVar 1])

        return fOut


-- | Like `groups` but use the given predicate to decide whether
--   consecutive values should be placed into the same group.
groupsBy :: (Value a -> Value a -> Value Bool) 
         -> Flow a -> Q (Flow (a :*: Int :*: ()))
groupsBy fun fIn
 = do   fOut    <- newFlow

        addNode $ G.NodeOp
         $ G.FopGroupsI
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam   $ xLam 
                        $ let Value x = fun (Value $ xVar 0) (Value $ xVar 1)
                          in  x)

        return $ fOut

