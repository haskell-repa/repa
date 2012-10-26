
module Test where
import Data.Array.Repa.Flow             as F
import qualified Data.Vector.Unboxed    as U



testFlowUnflow :: U.Vector Int -> IO (U.Vector Int)
testFlowUnflow vec
 = do   f       <- flow vec
        unflow f


testMap :: U.Vector Int -> IO (U.Vector Int)
testMap vec
 = do   f       <- flow vec
        unflow  $ F.map (+ 1234) f


testMapMap :: U.Vector Int -> IO (U.Vector Int)
testMapMap vec
 = do   f       <- flow vec
        unflow  $ F.map (+ 1234) $ F.map (* 3456) $ f


testMapZip :: U.Vector Int -> U.Vector Int -> IO (U.Vector Int)
testMapZip vec1 vec2
 = do   f1      <- flow vec1
        f2      <- flow vec2
        unflow  $ F.zipWith (+) (F.map (* 5678) f1) f2


testFold   :: U.Vector Int -> IO Int
testFold vec1 
 = do   f1      <- flow vec1
        F.foldl (+) 0 f1


testFoldIndexs :: U.Vector Int -> U.Vector Int -> IO Int
testFoldIndexs vec1 vec2
 = do   f2      <- flow vec2
        F.foldl (+) 0
         $ F.map (* 2345) $ F.indexs vec1 f2


testPack :: U.Vector Int -> IO (U.Vector Int)
testPack vec1
 = do   f1      <- flow vec1
        f2      <- F.filter (> 0) f1
        unflow f2
