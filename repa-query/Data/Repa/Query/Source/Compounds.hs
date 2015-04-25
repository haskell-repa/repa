
module Data.Repa.Query.Source.Compounds
        ( xVar, xLam, xOp
        , makeScalarOp1
        , makeScalarOp2)
where
import Data.Repa.Query.Source.Builder
import qualified Data.Repa.Query.Graph          as G


xVar i          = G.XVar () i
xLam x          = G.XVal () (G.VLam () () x)
xOp s args      = G.XOp  () s args



makeScalarOp1 :: G.ScalarOp -> Value a -> Value c
makeScalarOp1 sop (Value x)
        = Value $ xOp sop [x]


makeScalarOp2 :: G.ScalarOp -> Value a -> Value b -> Value c
makeScalarOp2 sop (Value x1) (Value x2)
        = Value $ xOp sop [x1, x2]
