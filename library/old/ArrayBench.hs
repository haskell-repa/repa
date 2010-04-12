{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Array.Unboxed
import StdArrays

--import qualified DArray as DA

import Control.Exception (evaluate)
import System.Console.GetOpt
import qualified System.Random as R



import  Data.Array.Parallel.Unlifted  ((:*:)(..))
import qualified Data.Array.Parallel.Unlifted as U

import Control.Exception (evaluate)


import Bench.Benchmark
import Bench.Options
import Debug.Trace



algs = [ ("1", mm1) 
       , ("2", mm2) 
       ]

{-
hhmDT:: (Int, U.Array Double) -> U.Array Double
hhmDT (n,arrData) = 
  res 
  where
  res = A.arrayData arr
  arr = fromDArray $ hhmDA $ hhmmult  (toHHMatrix n 32 a1)(toHHMatrix n 128 a1)
  a1  = A.toArray (() :*: n*n) arrData
  a2  = A.toArray (() :*: n*n) arrData
-}

mm1 :: (Int, (Array (Int, Int) Double)) -> Array (Int, Int) Double
mm1 (n, arr) = 
--   trace ("arr(0,0) = " ++ show (foldr max 0.0 $ [res!k | k <- range ((1,1), (n,n))]))  
   res 
   where
     res = (matMult arr arr)

mm2 :: (Int, (Array (Int, Int) Double)) -> Array (Int, Int) Double
mm2 (n, arr) = 
   trace ("arr(0,0) = " ++ show (foldr max 0.0 $ [res!k | k <- range ((1,1), (n,n))]))  res 
   where
     res = (matMult' arr arr)


generatePoints :: Int -> IO (Point (Int, (Array (Int,Int) Double)))
generatePoints n =
  do 
    let pts = (listArray ((1,1), (n,n)) [1..(fromIntegral $ n*n)])
    evaluate $ force pts 
    return $  ("N = " ++ show n) `mkPoint` (n, pts)
  where
    force pts = pts ! (1,1)


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
                          benchmark opts mm1
                             (Prelude.map generatePoints szs)
                             (`seq` ()) show
                          return ()

             Just f  -> do
                          benchmark opts f
                             (Prelude.map generatePoints szs)
                             (`seq` ()) show
                          return ()


