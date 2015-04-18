{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Repa.Query.Source.EDSL
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
        , (>),  (>=), (<), (<=)

        , get1, get2, get3, get4, get5)
where
import Control.Monad.State.Strict
import Data.Repa.Query.Source.Builder
import Data.Repa.Product
import qualified Data.Repa.Query.Graph  as G
import qualified Data.Repa.Query.Format as Format
import qualified Prelude                as P
import Prelude   
 hiding ( map, filter
        , (+),  (-), (*), (/)
        , (==), (/=)
        , (>),  (>=), (<), (<=))


---------------------------------------------------------------------------------------------------
-- | Source a named table.
source  :: String 
        -> Format.Delim 
        -> Format.Field a 
        -> Q (Flow a)

source table delim field
 = do   fOut    <- newFlow
        addNode $ G.NodeSource 
                $ G.SourceTable () table delim (Format.flattens field) 
                $ takeFlow fOut
        return  fOut


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
groups :: Flow a -> Q (Flow (a :*: Int))
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
         -> Flow a -> Q (Flow (a :*: Int))
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


-- Pairing and projection.
-- (**) :: Value a -> Value b -> Value (a :*: b)
-- (**) = 

get1   :: Value (a :*: b)                         -> Value a
get1    = makeScalarOp1 $ G.SopProj 1

get2   :: Value (a :*: (b :*: c))                 -> Value b
get2    = makeScalarOp1 $ G.SopProj 2

get3   :: Value (a :*: (b :*: (c :*: d)))         -> Value c
get3    = makeScalarOp1 $ G.SopProj 3

get4   :: Value (a :*: b :*: c :*: d :*: e)       -> Value d
get4    = makeScalarOp1 $ G.SopProj 4

get5   :: Value (a :*: b :*: c :*: d :*: e :*: f) -> Value e
get5    = makeScalarOp1 $ G.SopProj 5



makeScalarOp1
        :: G.ScalarOp 
        -> Value a -> Value c

makeScalarOp1 sop (Value x)
        = Value $ xOp sop [x]


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
 negate         = error "edsl finish me"
 abs            = error "edsl finish me"
 signum         = error "edsl finish me"
 fromInteger x  = Value $ G.XVal () (G.VLit () (G.LInt x))


---------------------------------------------------------------------------------------------------
xVar i          = G.XVar () i
xLam x          = G.XVal () (G.VLam () () x)
xOp s args      = G.XOp  () s args


