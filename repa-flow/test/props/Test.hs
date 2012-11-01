
module Main where
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import System.IO.Unsafe
import Data.Array.Repa.Flow             (Flow)
import qualified Data.Array.Repa.Flow   as F
import qualified Data.Vector.Unboxed    as U
import Prelude                          as P


-- Framework ------------------------------------------------------------------
main = defaultMain tests

tests
 =      [ testProperty "flow/unflow          " prop_flow_unflow
        , testProperty "replicate            " prop_replicate
        , testProperty "enumFromN            " prop_enumFromN
        , testProperty "map                  " prop_map
        , testProperty "replicates           " prop_replicates
        ]

instance (U.Unbox a, Arbitrary a) => Arbitrary (U.Vector a) where
 arbitrary
  = do  elems   <- arbitrary
        return  $ U.fromList elems


-- Computation / Conversion ---------------------------------------------------
prop_flow_unflow :: U.Vector Int -> Bool
prop_flow_unflow vec
 = unsafePerformIO
 $ do   ff      <- F.flow vec
        vec'    <- F.unflow ff
        return  $ vec' == vec


prop_replicate   :: Positive Int -> Int -> Bool
prop_replicate (Positive len) x
 = unsafePerformIO
 $ do   let len'  = len `mod` 1000
        ff      <- F.replicate len' x
        vec'    <- F.unflow ff
        return  $ vec' == U.replicate len' x


prop_enumFromN  :: Int -> Positive Int -> Bool
prop_enumFromN x (Positive len)
 = unsafePerformIO
 $ do   let len'  = len `mod` 1000
        ff      <- F.enumFromN x len'
        vec'    <- F.unflow ff
        return  $ vec' == U.enumFromN x len'


prop_map :: U.Vector Int -> Bool
prop_map vec
 = unsafePerformIO
 $ do   ff      <- F.flow vec
        vec'    <- F.unflow $ F.map (+ 1234) ff
        return  $ vec' == U.map (+ 1234) vec


prop_replicates :: U.Vector Int -> U.Vector Int -> Bool
prop_replicates lens0 vec0
 = unsafePerformIO
 $ do   let maxRepl     = 100
        let (lens, vec) = U.unzip $ U.zip (U.map (`mod` maxRepl) lens0) vec0
        ff              <- F.replicatesUnboxed (U.sum lens) lens vec
        vec'            <- F.unflow ff
        return $ U.toList vec'
             ==  P.concat (P.zipWith P.replicate (U.toList lens) (U.toList vec))
