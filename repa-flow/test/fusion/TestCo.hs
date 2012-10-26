{-# LANGUAGE BangPatterns #-}
module TestCo 
where
import Data.Array.Repa.CoFlow           as F
import qualified Data.Vector.Unboxed    as U


testFlowUnflow :: U.Vector Int -> IO (U.Vector Int)
testFlowUnflow vec
 = do   f       <- flow vec
        unflow f


testPack :: U.Vector Int -> IO (U.Vector Int)
testPack vec1
 = do   f1      <- flow vec1
        unflow $ F.filter (> 0) f1


testFold :: U.Vector Int -> IO Int
testFold !vec1 
 = do   f1      <- flow vec1
        F.foldl (+) 0 f1

