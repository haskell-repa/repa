module TestPar 
where
import GHC.Exts
import Data.Vector.Unboxed              (Vector)
import Data.Array.Repa.Flow.Par         as F
import Data.Array.Repa.Flow.Par.Segd    as Segd
import qualified Data.Vector.Unboxed    as U


testFlowUnflow :: U.Vector Int -> U.Vector Int
testFlowUnflow vec
 =      unflow (flow vec)


-- 1234
testMap :: U.Vector Int -> U.Vector Int
testMap vec
 =      unflow  $ F.map (+ 1234) (flow vec)

-- 2345
testMapMap :: U.Vector Int -> U.Vector Int
testMapMap vec
 =      unflow  $ F.map (+ 1234) $ F.map (* 2345) (flow vec)


-- 3456
testMapZip :: U.Vector Int -> U.Vector Int -> U.Vector Int
testMapZip vec1 vec2
 =      unflow  $ F.zipWith (+) (F.map (* 3456) (flow vec1)) (flow vec2)


-- 2323
testPackByTag :: U.Vector (Int, Int) -> U.Vector Int
testPackByTag vec1
 =      unflow  $ F.map (+ 2323) 
                $ F.packByTag (flow vec1)


-- 3434
testReplicates :: U.Vector Int -> U.Vector Int -> U.Vector Int
testReplicates !vLens !vElems
 =      unflow  $ F.map (+ 3434) 
                $ F.replicatesUnboxed (Segd.fromLengths vLens) vElems
