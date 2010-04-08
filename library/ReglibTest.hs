{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
 



import Control.Exception (evaluate)
import System.Console.GetOpt
import qualified System.Random as R


import qualified Data.Array.Parallel.Unlifted as U
import  Data.Array.Parallel.Unlifted  ((:*:)(..))


import qualified Array as A
import Array ((:.)(..))
import qualified ArrayExamples as AE 
import qualified DArray as DA
import qualified DArrayExamples as DAE
import  DArrayExamples  (Complex (..))



import Foo



import Control.Exception (evaluate)


import Bench.Benchmark
import Bench.Options

  
import Debug.Trace

default (Int, Float)

algs = [ ("1", transposeTest1)
       , ("2", transposeTest2)
       , ("3", transposeTest3)
       , ("4", appendTest1)
       , ("5", appendTest2)
       , ("6", tileTest)
       , ("7", mmMultTest)
       , ("8", lmMultTest)
       , ("9", fft3d)
       , ("11", mfTest)
       , ("14",  redBlack)
       , ("15",  redBlack2D)
       , ("16",  sumT)
       , ("17",  replicateT)
--       , ("4", relaxT)
--       , ("5", relaxShiftT)
--        , ("4", backpermuteDftT)
--        , ("7", mmT)
--        , ("8", replicateT)
--        , ("9", sumT)
--        , ("10", selectT)
--        , ("11", lmmT)
       ]
  

transposeTest1:: (Int, U.Array Double) -> U.Array Double
transposeTest1 (n, arrData) = trace (
    if (res == arrData) 
      then "OK: transpose . transpose = id"
      else "Error:  transpose . transpose /= id")
  res
  where  
    res = A.arrayData $ AE.transpose $ AE.transpose arr
    arr = A.toArray (() :. (n:: Int) :. (n::Int))  arrData
 
transposeTest2:: (Int, U.Array Double) -> U.Array Double
transposeTest2 (n, arrData) = trace (
    if (res == arrData) 
      then "OK: transposeDFT . transposeDFT = id"
      else "Error:  transposeDFT . transposeDFT /= id")
  res
  where  
    res = A.arrayData $ AE.transposeDFT $ AE.transposeDFT arr
    arr = A.toArray (() :. (n:: Int) :. (n::Int))  arrData

transposeTest3:: (Int, U.Array Double) -> U.Array Double
transposeTest3 (n, arrData) = trace (
    if (res == arrData) 
      then "OK: transposePrim . transposePrim = id"
      else "Error:  trans`posePrim . transposePrim /= id")
  res 
  where  
    res = A.arrayData $ AE.transposeDFT $ AE.transposeDFT arr
    arr = A.toArray (() :. (n:: Int) :. (n::Int))  arrData

appendTest1:: (Int, U.Array Double) -> U.Array Double
appendTest1 _ = trace (
   if (res == expected) 
      then "OK"
      else "Error: " ++ (show res))
  res 

  where 
    res = A.arrayData $ DA.fromDArray $ DA.append arr1 arr2 ((() :. (2::Int)) :. (8::Int))
    expected = U.fromList [1,2,3,4,5,1,2,3,6,7,8,9,10,4,5,6]
    arr1 = DA.toDArray $ A.toArray ((() :. (2::Int)) :. (5::Int)) $ U.fromList [1..10]
    arr2 = DA.toDArray $ A.toArray ((() :. (2::Int)) :. (3::Int)) $ U.fromList [1..6]


appendTest2:: (Int, U.Array Double) -> U.Array Double
appendTest2 _ = trace (
   if (res == expected) 
      then "OK"
      else "Error: " ++ (show res))
  res 
 
  where 
    expected = U.fromList [1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6]
    res = A.arrayData $ DA.fromDArray $ DA.append arr1 arr2 ((() :. (8::Int)) :. (2::Int))
    arr1 = DA.toDArray $ A.toArray ((() :. (5::Int)) :. (2::Int)) $ U.fromList [1..10]
    arr2 = DA.toDArray $ A.toArray ((() :. (3::Int)) :. (2::Int)) $ U.fromList [1..6]


  
tileTest::  (Int, U.Array Double) -> U.Array Double
tileTest _ = trace (show res) res
  where 
    res = A.arrayData $ DA.fromDArray $ DA.tile arr1 ((() :. 0 :. 0)::A.DIM2)     ((() :. 2 :. 2)::A.DIM2)
    arr1 = DA.toDArray $ A.toArray ((() :. (4::Int)):. (4::Int)) $ U.fromList [1..16]

  


lmMultTest:: (Int, U.Array Double) -> U.Array Double
lmMultTest (n, arr) = trace( 
  if (res == arr)
    then ("lmMult test ok, m * id = m\n")
    else ("lmMult test error, m * id /= m\n")) 
  res
  where   
    res = A.arrayData $ DAE.mmMult' arr1 arr2
    arr1 = DA.toDArray $ A.toArray ((() :. (n::Int)):. (n::Int)) arr
    arr2 = DA.DArray ((() :. (n::Int)):. (n::Int)) 
                (\(() :. i :. j) -> if (i==j) then 1 else 0)


mmMultTest:: (Int, U.Array Double) -> U.Array Double
mmMultTest (n, arr) = trace( 
  if (res == arr)
    then ("mmMult test ok, m * id = m\n")
    else ("mmMult test error, m * id /= m\n"))
  res
  where 
    res = A.arrayData $ AE.mmMult arr1 arr2
    arr1 = A.toArray ((() :. (n::Int)):. (n::Int)) arr
    arr2 = DA.fromDArray $ DA.DArray ((() :. (n::Int)):. (n::Int)) 
                (\(() :. i :. j) -> if (i==j) then 1 else 0)

mfTest:: (Int, U.Array Double) -> U.Array Double
mfTest _ = trace (show res) res
  where 
    res = A.arrayData $ DA.fromDArray $ DA.fold (+) 0 arr1
    arr1 = DA.toDArray $ A.toArray ((() :. (2::Int)):. (8::Int)) $ U.fromList [1..16]
    

sumT:: (Int, U.Array Double)  -> U.Array Double
sumT n = trace (show res)
  res
  where
    res = A.arrayData arr
    arr:: A.Array (() :. Int) Double
    arr = A.sum  (A.toArray ((() :. 5) :. 2) (U.fromList ([1..10]::[Double])))


{-
transposePT:: (Int, U.Array Int) -> U.Array Int
transposePT (n,arrData) = 
  res
  where  
    res = A.arrayData $ A.transposePrim arr
    arr = A.toArray (() :. (n:: Int) :. (n::Int))  arrData

transposeDFT:: (Int, U.Array Int) -> U.Array Int
transposeDFT (n,arrData) = 
  res
  where  
    res = A.arrayData $ A.transposeDFT arr
    arr = A.toArray (() :. (n:: Int) :. (n::Int)) arrData


-- insert an array into one twice the size
backpermuteDftT:: Int -> U.Array Int
backpermuteDftT n = --trace (show res) res
  res
  where
    res = A.arrayData $ A.backpermuteDft arr  0 (() :. 2*n) fn
    fn:: A.DIM1 -> Maybe A.DIM1
    fn (() :. m) = if m < n then Just (() :. m) else Nothing
    arr = A.toArray (() :. n)  (U.fromList ([1..n]::[Int]))


relaxT:: (Int, U.Array Int) -> U.Array Int
relaxT (n, arrData) = --trace (show res) res
  res
  where
    res = A.arrayData $ A.relax arr
    arr = A.toArray ((() :. n) :. n) arrData

relaxShiftT:: (Int, U.Array Int) -> U.Array Int
relaxShiftT (n, arrData) = --trace (show res) res
  res
  where
    res = A.arrayData $ A.relaxShift arr
    arr = A.toArray ((() :. n) :. n) arrData 



selectT:: Int -> U.Array Int
selectT n = 
  res 
  where
    res = A.arrayData arr
    arr = A.select (A.toArray ((() :. n) :. n) (U.fromList ([1..(n*n)]::[Int])))
                 (A.IndexFixed 0 (A.IndexFixed (n-1) A.IndexNil))


sumT:: Int -> U.Array Int
sumT n = trace (show res)
  res
  where
    res = A.arrayData arr
    arr:: A.Array (() :. Int) Int
    arr = A.mapFold (+) 0  (A.toArray ((() :. 5) :. 2) (U.fromList ([1..n*2*5]::[Int])))


mmT:: Int -> U.Array Int
mmT n = -- trace (show res)
  res 
  where
  res = A.arrayData arr
  arr = A.mmMult  a1 a2
  a1 = A.toArray (() :. n :. n) (U.fromList [1..n])
  a2 = A.toArray (() :. n :. n) (U.fromList [1..n])

-}

generatePoints :: Int -> IO (Point (Int, (U.Array Double)))
generatePoints n =
  do 
    let pts = (U.fromList [1..(fromIntegral $ n*n)])
    evaluate $ force pts 
    return $  ("N = " ++ show n) `mkPoint` (n, pts)
  where
    force pts = pts U.!: n

{-
lmmT:: Int -> U.Array Int
lmmT n = -- trace (show res)
  res 
  where
  res = A.arrayData arr
  arr = DA.mmMult  a1 a2
  a1 = A.toArray (() :. n :. n) (U.fromList [1..n])
  a2 = A.toArray (() :. n :. n) (U.fromList [1..n])
  -}


redBlack:: (Int, U.Array Double) -> U.Array Double 
redBlack _ =
  trace (show result) result
  where
    result = A.arrayData $ DA.fromDArray $ DAE.redBlack 1 0 a1 a2
    a1  = DA.DArray (() :. 4 :. 4 :. 4) (\_ -> 1)
    a2  = DA.DArray (() :. 4 :. 4 :. 4) (\_ -> 1)


redBlack2D:: (Int, U.Array Double) -> U.Array Double 
redBlack2D (n,arr) =
  trace (show result) result
  where
    result = A.arrayData $ DA.fromDArray $ DAE.redBlackChecker2D 1000 1 0 a a
    a  = DA.toDArray $ A.toArray ((() :. (n::Int)):. (n::Int)) arr


fft3d:: (Int, (U.Array Double)) -> U.Array Double
fft3d (size, (arr2)) =  trace ("\n res= " ++  
     (show res)  ++ "\n arg = " ++ 
     (show arr)++ "\n rofu =" ++
     (show rofu)++ "\n resList =" ++
      (show $ map (\(r :*: _) -> r) $ fftList arrList rofuList )++ "\n  res =" ++
     (show $ A.arrayData $ DA.fromDArray rofu) ) res
  where
    rofuList = U.toList $ A.arrayData $ DA.fromDArray rofu
    arrList = U.toList $ A.arrayData $ DA.fromDArray arr

    res = A.arrayData $ DA.fromDArray $ DA.map (\(r :*: i) -> r) $ DAE.fft rofu arr
    rofu = (DAE.calcRofu (() :. (size `div`2)))
    arr =  DA.toDArray $ A.toArray (() :. size) (sinSeq size)


sinSeq:: Int -> U.Array (Double :*: Double)
sinSeq n = U.fromList $ map (\x -> (sin (x * (2 * pi)/nD) :*: 0.0)) [1.0..nD]
  where nD = fromIntegral n


fftList:: [Complex] -> [Complex] -> [Complex] 
fftList (c1 : c2 : []) (r:[]) = [c1 + c2, c1 - c2]
fftList cs rofu =  (zipWith (+) fftL fftR) ++ (zipWith (-) fftL fftR)
  where
    fftL = (fftList cL rofu')
    fftR = zipWith (*) rofu (fftList cR rofu')
    rofu' = splitRofu rofu
    (cL, cR) = split cs
    split [] = ([],[])
    split (x:y:rs) = (x:rsL, y :rsR) 
       where (rsL, rsR) = split (rs)
    splitRofu [] = []
    splitRofu (r1:r2:rs) = r1 : (splitRofu rs) 

replicateT:: (Int, U.Array Double) -> U.Array Double 
replicateT (n, arr) = trace (show res)
  res 
  where
    res = A.arrayData $ DA.fromDArray arr
    arr = DA.replicateSlice  (DA.toDArray $ (A.toArray (() :. 10) (U.fromList ([1.0..10.0]::[Double]))))
                 (() :. A.All :. (3::Int))
{-
    Arr = A.replicate  (A.toArray (() :. 10) (U.fromList ([1.0..10.0]::[Double])))
                 (A.IndexFixed 3 (A.IndexAll (A.IndexNil)))
  -}
 
main = ndpMain "RArray Test"
               "[OPTION] ... SIZES ..."
               run [Option ['a'] ["algo"] (ReqArg const "ALGORITHM")
                     "use the specified algorithm"]
                   "seq" 




run opts alg sizes =
  case Prelude.map read sizes of
    []  -> failWith ["No sizes specified"]
    szs -> case lookup alg algs of
             Nothing -> do
                          benchmark opts transposeTest1
                             (Prelude.map generatePoints szs)
                             (`seq` ()) show
                          return ()

             Just f  -> do
                          benchmark opts f
                             (Prelude.map generatePoints szs)
                             (`seq` ()) show
                          return ()


