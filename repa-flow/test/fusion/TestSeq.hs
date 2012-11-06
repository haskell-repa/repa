{-# LANGUAGE MagicHash, BangPatterns #-}
module TestSeq
where
import GHC.Exts
import Data.Vector.Unboxed              (Vector)
import Data.Array.Repa.Flow.Seq         as F
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


testMapZip :: U.Vector Int -> U.Vector Int -> U.Vector Int
testMapZip vec1 vec2
 =      unflow  $ F.zipWith (+) (F.map (* 5678) (flow vec1)) (flow vec2)


testFold :: U.Vector Int -> Int
testFold !vec1 
 =      F.foldl (+) 0 $ F.map (+ 2345) (flow vec1)


testFoldGather :: U.Vector Int -> U.Vector Int -> Int
testFoldGather vec1 vec2
 =      F.foldl (+) 0
         $ F.map (* 2345) $ F.gather vec1 (flow vec2)


-- TODO: This doesn't work well because we don't have a good way to convert
--       a bool to an Int without generating a case-expresison in the core code.
testFilter :: U.Vector Int -> U.Vector Int
testFilter vec1
 =      unflow  $ F.filter (> 1212) (flow vec1)


testPackByTag :: U.Vector (Int, Int) -> U.Vector Int
testPackByTag vec1
 =      unflow  $ F.map (+2323) 
                $ F.packByTag (flow vec1)


testReplicates :: Int -> U.Vector Int -> U.Vector Int -> U.Vector Int
testReplicates !(I# len) !vLens !vElems
 =      unflow  $ F.map (+ 3434) 
                $ F.replicatesUnboxed len vLens vElems



-- testFoldReplicate :: Int -> U.Vector Int -> U.Vector Int -> U.Vector Int
-- testFoldReplicate !(I# len) !vLens !vElems
--  =      unflow  $ F.sums (flow vLens)
--                $ F.map (+ 4545)
--                $ F.replicatesUnboxed len vLens vElems
