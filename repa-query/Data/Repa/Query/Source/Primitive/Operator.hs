
module Data.Repa.Query.Source.Primitive.Operator
        ( map, map2, map3, map4, map5, map6, map7
        , zip, zip2, zip3, zip4, zip5, zip6, zip7
        , fold, folds
        , filter
        , groups, groupsBy)
where
import Control.Monad.State.Strict
import Data.Repa.Query.Source.Primitive.Projection
import Data.Repa.Query.Source.Builder
import Data.Repa.Query.Graph.Compounds          as G
import qualified Data.Repa.Query.Graph          as G
import Data.Repa.Scalar.Product
import Data.Int
import Prelude   
 hiding ( map
        , zip, zip3
        , filter
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
                 $ let Value x  = fun   (Value $ xVar 1) (Value $ xVar 0)
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
                 $ let Value x  = fun   (Value $ xVar 2) (Value $ xVar 1)
                                        (Value $ xVar 0)
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
                 $ let Value x  = fun   (Value $ xVar 3) (Value $ xVar 2)
                                        (Value $ xVar 1) (Value $ xVar 0)
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
                 $ let Value x  = fun   (Value $ xVar 4) (Value $ xVar 3)
                                        (Value $ xVar 2) (Value $ xVar 1)
                                        (Value $ xVar 0)
                   in  x)

        return fOut


-- | Apply a scalar function to every element of a flow.
map6    :: (Value a -> Value b -> Value c -> Value d -> Value e -> Value f -> Value g) 
        -> Q (Flow a) -> Q (Flow b) -> Q (Flow c) -> Q (Flow d) -> Q (Flow e) -> Q (Flow f)
        -> Q (Flow g)

map6 fun mkIn1 mkIn2 mkIn3 mkIn4 mkIn5 mkIn6
 = do   fIn1    <- mkIn1
        fIn2    <- mkIn2
        fIn3    <- mkIn3
        fIn4    <- mkIn4
        fIn5    <- mkIn5
        fIn6    <- mkIn6
        fOut    <- newFlow

        addNode $ G.NodeOp 
         $ G.FopMapI 
                [ takeFlow fIn1, takeFlow fIn2, takeFlow fIn3, takeFlow fIn4
                , takeFlow fIn5, takeFlow fIn6 ]
                (takeFlow fOut)
                (xLam $ xLam $ xLam $ xLam $ xLam $ xLam
                 $ let Value x  = fun   (Value $ xVar 5)
                                        (Value $ xVar 4) (Value $ xVar 3)
                                        (Value $ xVar 2) (Value $ xVar 1)
                                        (Value $ xVar 0)
                   in  x)

        return fOut


-- | Apply a scalar function to every element of a flow.
map7    :: (Value a -> Value b -> Value c -> Value d -> Value e -> Value f -> Value g -> Value h) 
        -> Q (Flow a) -> Q (Flow b) -> Q (Flow c) -> Q (Flow d) 
        -> Q (Flow e) -> Q (Flow f) -> Q (Flow g)
        -> Q (Flow h)

map7 fun mkIn1 mkIn2 mkIn3 mkIn4 mkIn5 mkIn6 mkIn7
 = do   fIn1    <- mkIn1
        fIn2    <- mkIn2
        fIn3    <- mkIn3
        fIn4    <- mkIn4
        fIn5    <- mkIn5
        fIn6    <- mkIn6
        fIn7    <- mkIn7
        fOut    <- newFlow

        addNode $ G.NodeOp 
         $ G.FopMapI 
                [ takeFlow fIn1, takeFlow fIn2, takeFlow fIn3, takeFlow fIn4
                , takeFlow fIn5, takeFlow fIn6, takeFlow fIn7 ]
                (takeFlow fOut)
                (xLam $ xLam $ xLam $ xLam $ xLam $ xLam $ xLam
                 $ let Value x  = fun   (Value $ xVar 6) (Value $ xVar 5)
                                        (Value $ xVar 4) (Value $ xVar 3)
                                        (Value $ xVar 2) (Value $ xVar 1)
                                        (Value $ xVar 0)
                   in  x)

        return fOut


----------------------------------------------------------------------------------------------- Zip
-- | Zip a flow of elements into a row.
zip     :: Q (Flow a)
        -> Q (Flow (a :*: ()))
zip = map row1


-- | Zip corresponding elements of two flows.
zip2    :: Q (Flow a) -> Q (Flow b) 
        -> Q (Flow (a :*: b :*: ()))
zip2 = map2 row2


-- | Zip corresponding elements of three flows.
zip3    :: Q (Flow a) -> Q (Flow b) -> Q (Flow c)
        -> Q (Flow (a :*: b :*: c :*: ()))
zip3 = map3 row3


-- | Zip corresponding elements of four flows.
zip4    :: Q (Flow a) -> Q (Flow b) -> Q (Flow c) -> Q (Flow d)
        -> Q (Flow (a :*: b :*: c :*: d :*: ()))
zip4 = map4 row4


-- | Zip corresponding elements of five flows.
zip5    :: Q (Flow a) -> Q (Flow b) -> Q (Flow c) -> Q (Flow d) -> Q (Flow e)
        -> Q (Flow (a :*: b :*: c :*: d :*: e :*: ()))
zip5 = map5 row5


-- | Zip corresponding elements of six flows.
zip6    :: Q (Flow a) -> Q (Flow b) -> Q (Flow c) -> Q (Flow d) -> Q (Flow e) -> Q (Flow f)
        -> Q (Flow (a :*: b :*: c :*: d :*: e :*: f :*: ()))
zip6 = map6 row6


-- | Zip corresponding elements of three flows.
zip7    :: Q (Flow a) -> Q (Flow b) -> Q (Flow c) -> Q (Flow d) 
        -> Q (Flow e) -> Q (Flow f) -> Q (Flow g)
        -> Q (Flow (a :*: b :*: c :*: d :*: e :*: f :*: g :*: ()))
zip7 = map7 row7


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

