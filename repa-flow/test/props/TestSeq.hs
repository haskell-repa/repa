
module Main where
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import System.IO.Unsafe
import Data.Array.Repa.Flow.Seq                 (Flow)
import qualified Data.Array.Repa.Flow.Seq       as F
import qualified Data.Vector.Unboxed            as U
import Prelude                                  as P
import GHC.Exts

-- Framework ------------------------------------------------------------------
main = defaultMain tests

tests
 =      [ testProperty "flow/unflow          " prop_flow_unflow
        , testProperty "map                  " prop_map
        , testProperty "map/map              " prop_map_map
        , testProperty "replicate            " prop_replicate
        , testProperty "replicates           " prop_replicates
        , testProperty "enumFromN            " prop_enumFromN
        , testProperty "filter               " prop_filter
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
   in   U.toList (F.unflow $ F.replicatesUnboxed total lens vec)
    ==  P.concat (P.zipWith P.replicate (U.toList lens) (U.toList vec))


prop_filter :: U.Vector Int -> Bool
prop_filter vec
 =      U.filter (\x -> x `mod` 2 == 0) vec
     == F.unflow (F.filter (\x -> x `mod` 2 == 0) (F.flow vec))

