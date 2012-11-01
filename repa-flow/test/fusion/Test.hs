{-# LANGUAGE BangPatterns #-}
module Test 
where
import Data.Array.Repa.Flow             as F
import qualified Data.Vector.Unboxed    as U


testFlowUnflow :: U.Vector Int -> IO (U.Vector Int)
testFlowUnflow vec
 = do   f       <- flow vec
        unflow f

-- 1234
testMap :: U.Vector Int -> IO (U.Vector Int)
testMap vec
 = do   f       <- flow vec
        unflow  $ F.map (+ 1234) f


-- 2345
testMapMap :: U.Vector Int -> IO (U.Vector Int)
testMapMap vec
 = do   f       <- flow vec
        unflow  $ F.map (+ 1234) $ F.map (* 2345) $ f

testMapZip :: U.Vector Int -> U.Vector Int -> IO (U.Vector Int)
testMapZip vec1 vec2
 = do   f1      <- flow vec1
        f2      <- flow vec2
        unflow  $ F.zipWith (+) (F.map (* 5678) f1) f2


testFold :: U.Vector Int -> IO Int
testFold !vec1 
 = do   f1      <- flow vec1
        F.foldl (+) 0 $ F.map (+ 2345) f1


testFoldGather :: U.Vector Int -> U.Vector Int -> IO Int
testFoldGather vec1 vec2
 = do   f2      <- flow vec2
        F.foldl (+) 0
         $ F.map (* 2345) $ F.gather vec1 f2


-- TODO: This doesn't work well because we don't have a good way to convert
--       a bool to an Int without generating a case-expresison in the core code.
testFilter :: U.Vector Int -> IO (U.Vector Int)
testFilter vec1
 = do   f1      <- flow vec1
        f2      <- F.filter (> 1212) f1
        unflow f2


testPackByTag :: U.Vector (Int, Int) -> IO (U.Vector Int)
testPackByTag vec1
 = do   f1      <- flow vec1
        f2      <- F.packByTag f1
        unflow $ F.map (+2323) f2


testReplicates :: Int -> U.Vector Int -> U.Vector Int -> IO (U.Vector Int)
testReplicates !len !vLens !vElems
 = do   ff      <- F.replicatesUnboxed len vLens vElems
        unflow $ F.map (+ 3434) ff


testFoldReplicate :: Int -> U.Vector Int -> U.Vector Int -> IO (U.Vector Int)
testFoldReplicate !len !vLens !vElems
 = do   fLens   <- F.flow vLens
        ff      <- F.replicatesUnboxed len vLens vElems
        unflow $ F.sums fLens $ F.map (+ 4545) ff
