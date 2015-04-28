
module Data.Repa.Query.Source.Operator
        ( map, map2, map3, map4, map5
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


----------------------------------------------------------------------------------------------- Map
-- | Apply a scalar function to every element of a flow.
map :: (Value a -> Value b) -> Q (Flow a) -> Q (Flow b)
map fun mkIn
 = do   fIn     <- mkIn
        fOut    <- newFlow

        addNode $ G.NodeOp 
         $ G.FopMapI 
                [takeFlow fIn]
                (takeFlow fOut)
                (xLam $ let Value x  = fun $ Value $ xVar 0
                        in  x)

        return fOut


-- | Apply a scalar function to every element of a flow.
map2    :: (Value a -> Value b -> Value c) 
        -> Q (Flow a) -> Q (Flow b)
        -> Q (Flow c)

map2 fun mkIn1 mkIn2
 = do   fIn1    <- mkIn1
        fIn2    <- mkIn2
        fOut    <- newFlow

        addNode $ G.NodeOp 
         $ G.FopMapI 
                [takeFlow fIn1, takeFlow fIn2]
                (takeFlow fOut)
                (xLam $ xLam 
                 $ let Value x  = fun   (Value $ xVar 0) (Value $ xVar 1)
                   in  x)

        return fOut


-- | Apply a scalar function to every element of a flow.
map3    :: (Value a -> Value b -> Value c -> Value d)
        -> Q (Flow a) -> Q (Flow b) -> Q (Flow c)
        -> Q (Flow d)

map3 fun mkIn1 mkIn2 mkIn3
 = do   fIn1    <- mkIn1
        fIn2    <- mkIn2
        fIn3    <- mkIn3
        fOut    <- newFlow

        addNode $ G.NodeOp 
         $ G.FopMapI 
                [takeFlow fIn1, takeFlow fIn2, takeFlow fIn3]
                (takeFlow fOut)
                (xLam $ xLam $ xLam
                 $ let Value x  = fun   (Value $ xVar 0) (Value $ xVar 1)
                                        (Value $ xVar 2)
                   in  x)

        return fOut


-- | Apply a scalar function to every element of a flow.
map4    :: (Value a -> Value b -> Value c -> Value d -> Value e) 
        -> Q (Flow a) -> Q (Flow b) -> Q (Flow c) -> Q (Flow d)
        -> Q (Flow e)

map4 fun mkIn1 mkIn2 mkIn3 mkIn4
 = do   fIn1    <- mkIn1
        fIn2    <- mkIn2
        fIn3    <- mkIn3
        fIn4    <- mkIn4
        fOut    <- newFlow

        addNode $ G.NodeOp 
         $ G.FopMapI 
                [takeFlow fIn1, takeFlow fIn2, takeFlow fIn3, takeFlow fIn4]
                (takeFlow fOut)
                (xLam $ xLam $ xLam $ xLam
                 $ let Value x  = fun   (Value $ xVar 0) (Value $ xVar 1)
                                        (Value $ xVar 2) (Value $ xVar 3)
                   in  x)

        return fOut


-- | Apply a scalar function to every element of a flow.
map5    :: (Value a -> Value b -> Value c -> Value d -> Value e -> Value f) 
        -> Q (Flow a) -> Q (Flow b) -> Q (Flow c) -> Q (Flow d) -> Q (Flow e)
        -> Q (Flow f)

map5 fun mkIn1 mkIn2 mkIn3 mkIn4 mkIn5
 = do   fIn1    <- mkIn1
        fIn2    <- mkIn2
        fIn3    <- mkIn3
        fIn4    <- mkIn4
        fIn5    <- mkIn5
        fOut    <- newFlow

        addNode $ G.NodeOp 
         $ G.FopMapI 
                [takeFlow fIn1, takeFlow fIn2, takeFlow fIn3, takeFlow fIn4, takeFlow fIn5]
                (takeFlow fOut)
                (xLam $ xLam $ xLam $ xLam $ xLam
                 $ let Value x  = fun   (Value $ xVar 0) (Value $ xVar 1)
                                        (Value $ xVar 2) (Value $ xVar 3)
                                        (Value $ xVar 4)
                   in  x)

        return fOut


---------------------------------------------------------------------------------------------- Fold 
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
        -> Q (Flow a) -> Q (Flow a)

fold fun (Value z) mkIn
 = do   fIn     <- mkIn
        fOut    <- newFlow

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
        -> Q (Flow (n :*: Int :*: ()))
        -> Q (Flow a)
        -> Q (Flow (n :*: a   :*: ()))

folds fun (Value z) mkLens mkElems
 = do   fLens   <- mkLens
        fElems  <- mkElems
        fOut    <- newFlow

        addNode $ G.NodeOp
         $ G.FopFoldsI
                (takeFlow fLens)
                (takeFlow fElems)
                (takeFlow fOut)
                (xLam $ xLam $ let Value x = fun (Value $ xVar 0) (Value $ xVar 1)
                               in  x)
                z

        return fOut


-------------------------------------------------------------------------------------------- Filter
-- | Keep only the elements that match the given predicate.
filter  :: (Value a -> Value Bool)
        -> Q (Flow a) -> Q (Flow a)

filter fun mkIn
 = do   fIn     <- mkIn
        fOut    <- newFlow

        addNode $ G.NodeOp
         $ G.FopFilterI
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam $ let Value x  = fun $ Value $ xVar 0
                        in  x)

        return fOut


--------------------------------------------------------------------------------------------- Group
-- | Scan through a flow to find runs of consecutive values,
--   yielding the value and the length of each run.
groups :: Q (Flow a) -> Q (Flow (a :*: Int :*: ()))
groups mkIn
 = do   fIn     <- mkIn
        fOut    <- newFlow

        addNode $ G.NodeOp  
         $ G.FopGroupsI
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam $ xLam $ xOp G.SopEq [xVar 0, xVar 1])

        return fOut


-- | Like `groups` but use the given predicate to decide whether
--   consecutive values should be placed into the same group.
groupsBy :: (Value a -> Value a -> Value Bool) 
         -> Q (Flow a) 
         -> Q (Flow (a :*: Int :*: ()))

groupsBy fun mkIn
 = do   fIn     <- mkIn
        fOut    <- newFlow

        addNode $ G.NodeOp
         $ G.FopGroupsI
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam   $ xLam 
                        $ let Value x = fun (Value $ xVar 0) (Value $ xVar 1)
                          in  x)

        return $ fOut

