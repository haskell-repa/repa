{-# LANGUAGE ScopedTypeVariables, MagicHash, BangPatterns, UnboxedTuples,
             ExistentialQuantification, TypeFamilies #-}
module Main where
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import System.IO.Unsafe
import Data.Array.Repa.Flow.Par                 (Flow)
import qualified Data.Array.Repa.Flow.Par.Segd  (Segd)
import qualified Data.Array.Repa.Flow.Par.Segd  as Segd
import qualified Data.Array.Repa.Flow.Par       as F
import qualified Data.Vector.Unboxed            as U
import Prelude                                  as P
import GHC.Exts


-- Framework ------------------------------------------------------------------
main = defaultMainWithArgs tests ["-j", "1"] 

tests
 =      [ testProperty "flow/unflow          " prop_flow_unflow
        , testProperty "map                  " prop_map
        , testProperty "map/map              " prop_map_map
        , testProperty "replicate            " prop_replicate
        , testProperty "enumFromN            " prop_enumFromN
        , testProperty "replicates           " prop_replicates
--      , testProperty "filter               " prop_filter
        ]

instance (U.Unbox a, Arbitrary a) => Arbitrary (U.Vector a) where
 arbitrary
  = do  elems   <- arbitrary
        return  $ U.fromList elems


-- Computation / Conversion ---------------------------------------------------
prop_flow_unflow :: U.Vector Int -> Bool
prop_flow_unflow vec
 =      vec == (F.unflow $ F.flow vec)


prop_replicate   :: Positive Int -> Int -> Bool
prop_replicate (Positive len) x
 = let  !len'@(I# len#)  = len `mod` 1000
   in   U.replicate len' x
     == F.unflow (F.replicate len# x)


prop_enumFromN  :: Int -> Positive Int -> Bool
prop_enumFromN (I# x) (Positive len)
 = let  !(I# len')  = len `mod` 1000
   in   U.enumFromN (I# x) (I# len')
     == F.unflow (F.enumFromN x len')


prop_map :: U.Vector Int -> Bool
prop_map vec
 =      U.map (+ 1234) vec
     == (F.unflow $ F.map (+ 1234) (F.flow vec))


prop_map_map :: U.Vector Int -> Bool
prop_map_map vec
 =      (U.map (+ 1234) $ U.map (* 4567) vec)
     == (F.unflow $ F.map (+ 1234) $ F.map (* 4567) (F.flow vec))


prop_replicates :: U.Vector Int -> U.Vector Int -> Bool
prop_replicates lens0 vec0
 = let  maxRepl         = 100
        (lens, vec)     = U.unzip $ U.zip (U.map (abs . (`mod` maxRepl)) lens0) vec0
        !(I# total)     = U.sum lens
        segd            = Segd.fromLengths lens
        getVal ix       = vec0 U.! (I# ix)
   in   U.toList (F.unflow $ F.replicates segd getVal)
    ==  P.concat (P.zipWith P.replicate (U.toList lens) (U.toList vec))


-- prop_filter :: U.Vector Int -> Bool
-- prop_filter vec
--  =      U.filter (\x -> x `mod` 2 == 0) vec
--      == F.unflow (F.filter (\x -> x `mod` 2 == 0) (F.flow vec))

