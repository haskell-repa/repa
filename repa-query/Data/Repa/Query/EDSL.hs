{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Repa.Query.EDSL
        ( -- * Variables
          Flow
        , Value

          -- * Flow operators
          -- | The provided operators are restricted to the set that can be
          --   performed on the fly, without needing to create intermediate
          --   tables.

          -- ** Sourcing
        , source

          -- ** Mapping
        , map

          -- ** Folding
        , fold
        , folds

          -- ** Filtering
        , filter

          -- ** Grouping
        , groups
        , groupsBy

          -- * Scalar operators
        , (+),  (-),  (*), (/)
        , (==), (/=)
        , (>),  (>=), (<), (<=))
where
import Control.Monad.State.Strict
import Data.Repa.Query.Builder
import qualified Data.Repa.Query.Graph  as G
import qualified Prelude                as P
import Prelude   
 hiding ( map, filter
        , (+),  (-), (*), (/)
        , (==), (/=)
        , (>),  (>=), (<), (<=))


---------------------------------------------------------------------------------------------------
-- | Source a named table.
-- 
--   When building an operator graph the format of the table is not known,
--   and hence the element type of the flow is polymorphic. 
--   If you want to manually constrain the flow to have a particular type
--   then provide an explicit type signature, For example:
--
-- @ 
--   (f :: Flow (Int :*: Int)) <- source "table"
-- @
--
--   When the query is compiled to executable code, the real format of the
--   table will be read from its metadata, and the type of the flow elements
--   checked against any provided signature.
--    
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
                (xLam $ let Value x = fun (Value $ xVar 0) (Value $ xVar 1)
                        in  x)
                z

        return fOut


-- | Segmented fold. Like `fold`, but combine consecutive runs of elements. 
--   The length of each run is taken from a second flow.
folds   :: (Value a -> Value a -> Value a)
        -> Value a
        -> Flow Int -> Flow a
        -> Q (Flow a)

folds fun (Value z) fLens fElems
 = do   fOut    <- newFlow

        addNode $ G.NodeOp
         $ G.FopFoldsI
                (takeFlow fLens)
                (takeFlow fElems)
                (takeFlow fOut)
                (xLam $ let Value x = fun (Value $ xVar 0) (Value $ xVar 1)
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
groups :: Flow a -> Q (Flow (a, Int))
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
         -> Flow a -> Q (Flow (a, Int))
groupsBy fun fIn
 = do   fOut    <- newFlow

        addNode $ G.NodeOp
         $ G.FopGroupsI
                (takeFlow fIn)
                (takeFlow fOut)
                (xLam   $ xLam 
                        $ let Value x = fun (Value $ xVar 0) (Value $ xVar 1)
                          in  x)

        return fOut


---------------------------------------------------------------------------------------------------
-- Wrappers for scalar operators.

-- | Scalar addition.
(+) :: Value a -> Value a -> Value a
(+) = makeScalarOp2 G.SopAdd 

-- | Scalar subtraction.
(-) :: Value a -> Value a -> Value a
(-) = makeScalarOp2 G.SopSub

-- | Scalar multiplication.
(*) :: Value a -> Value a -> Value a
(*) = makeScalarOp2 G.SopMul

-- | Scalar division.
(/) :: Value a -> Value a -> Value a
(/) = makeScalarOp2 G.SopMul

-- | Scalar equality.
(==) :: Value a -> Value a -> Value Bool
(==) = makeScalarOp2 G.SopEq

-- | Scalar negated equality.
(/=) :: Value a -> Value a -> Value Bool
(/=) = makeScalarOp2 G.SopNeq

-- | Scalar greater-than.
(>) :: Value a -> Value a -> Value Bool
(>) = makeScalarOp2 G.SopGt

-- | Scalar greater-than-equal.
(>=) :: Value a -> Value a -> Value Bool
(>=) = makeScalarOp2 G.SopGe

-- | Scalar less-than.
(<) :: Value a -> Value a -> Value Bool
(<)  = makeScalarOp2 G.SopLt

-- | Scalar less-than-equal.
(<=) :: Value a -> Value a -> Value Bool
(<=) = makeScalarOp2 G.SopLe


makeScalarOp2 
        :: G.ScalarOp 
        -> Value a -> Value b -> Value c

makeScalarOp2 sop (Value x1) (Value x2)
        = Value $ xOp sop [x1, x2]


---------------------------------------------------------------------------------------------------
instance Num (Value Int) where
 (+)            = makeScalarOp2 G.SopAdd
 (-)            = makeScalarOp2 G.SopSub
 (*)            = makeScalarOp2 G.SopMul
 negate         = undefined
 abs            = undefined
 signum         = undefined
 fromInteger x  = Value $ G.XLit () (G.LitInt x)


---------------------------------------------------------------------------------------------------
xVar i          = G.XVar () i
xLam x          = G.XLam () () x
xOp s args      = G.XOp  () s args


