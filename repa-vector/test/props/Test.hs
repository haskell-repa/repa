{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE ParallelListComp #-}

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.Array.Repa
import Data.Array.Repa.Vector
import System.IO.Unsafe
import qualified Data.Vector.Unboxed    as U
import Prelude                          as P


instance Num Z where
 Z + Z         = Z
 Z - Z         = Z
 Z * Z         = Z
 abs Z         = Z
 signum Z      = 0

 fromInteger 0 = Z
 fromInteger _ = error "fromInteger[Z]: non-zero integer"


instance Arbitrary Z where
 arbitrary = return Z
 

instance (Arbitrary a, Arbitrary (Positive b))
       => Arbitrary (a :. b) where
 arbitrary
  = do  x1              <- arbitrary
        Positive x2     <- arbitrary
        return (x1 :. x2)


instance (Arbitrary a, U.Unbox a)
      => Arbitrary (Vector U a) where
 arbitrary
  = do  elems           <- arbitrary
        return $ fromListUnboxed (Z :. length elems) elems

-- Framework ------------------------------------------------------------------
main = defaultMain tests

tests
 =      [ testProperty "compute/chain"          prop_compute_chain
        , testProperty "compute/stream"         prop_compute_stream
        , testProperty "compute/toStream/chain" prop_compute_toStream_chain
        , testProperty "replicate"              prop_replicate
        , testProperty "indexed/U"              prop_indexedU 
        , testProperty "indexed/N"              prop_indexedN
        , testProperty "pack"                   prop_pack
        ]


-- Computation / Conversion ---------------------------------------------------
prop_compute_chain :: Vector U Int -> Bool
prop_compute_chain vec
 = let result =  unsafePerformIO $ vcomputeUnboxedP (vchain vec)
   in  result == vec


prop_compute_stream :: Vector U Int -> Bool
prop_compute_stream vec
 = let result =  unsafePerformIO $ vcomputeUnboxedP (vstream vec)
   in  result == vec


prop_compute_toStream_chain :: Vector U Int -> Bool
prop_compute_toStream_chain vec
 = let  result = unsafePerformIO $ vcomputeUnboxedP (vstreamOfChain $ vchain vec)
   in   result == vec


-- Replicate ------------------------------------------------------------------
prop_replicate :: Positive Int -> Int -> Bool
prop_replicate (Positive len) val
 = let len'     = min len 10000
   in  replicate len' val
    == vtoList  (vreplicate len' val)


-- Indexed --------------------------------------------------------------------
prop_indexedU :: [Int] -> Bool
prop_indexedU xs
 =  [(i, x) | x <- xs | i <- [0..]]
 == (vtoList $ vindexed $ vfromListUnboxed xs)


prop_indexedN :: [Int] -> Bool
prop_indexedN xs
 =  [(i, x) | x <- xs | i <- [0..]]
 == (vtoList $ vindexed $ vchain $ vfromListUnboxed xs)


-- Pack -----------------------------------------------------------------------
prop_pack :: [(Bool, Int)] -> Bool
prop_pack xs
 =  (P.map snd $ filter fst xs)
 == (vtoList $ vpack $ vfromListUnboxed xs)

