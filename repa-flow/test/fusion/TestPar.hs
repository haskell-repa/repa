module TestPar 
where
import GHC.Exts
import Data.Vector.Unboxed              (Vector)
import Data.Array.Repa.Flow.Par         as F
import qualified Data.Vector.Unboxed    as U


testFlowUnflow :: U.Vector Int -> U.Vector Int
testFlowUnflow vec
 =      unflow (flow vec)


-- 1234
testMap :: U.Vector Int -> U.Vector Int
testMap vec
 =      unflow  $ F.map (+ 1234) (flow vec)

