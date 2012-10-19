{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE ParallelListComp, MagicHash, BangPatterns #-}

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck                  
import Data.Array.Repa
import Data.Array.Repa.Vector
import Data.Array.Repa.Vector.Segd              (Segd(Segd))
import System.IO.Unsafe
import qualified Data.Vector.Unboxed            as U
import qualified Data.Array.Repa.Vector.Segd    as Segd
import Prelude                                  as P
import Data.List
import GHC.Exts 


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


-- | Segment descriptor of length n.
-- 
--   Do not use directly unless an arbitrary Segd is all you need.  
--   Use segdForArray to generate a Segd that fits the array.
-- 
instance Arbitrary (Segd U U) where
 arbitrary 
  = sized $ \n@(I# n') 
  -> do ids             <- genIndices n
        let lens        = indicesToLengths ids n
        return $ Segd (vfromListUnboxed lens) (vfromListUnboxed ids) n'
  where 
        -- list of non-decreasing integers in range [0, n)
        genIndices 0 = return []
        genIndices n = ((0:) . sort . P.map (`mod` n)) `fmap` arbitrary
        indicesToLengths ids n = P.zipWith (-) (tail $ ids P.++ [n]) ids


-- | Generate a segment descriptor fitting the given vector
segdForVector :: Source r a => Vector r a -> Gen (Segd U U)
segdForVector arr = resize (vlength arr) arbitrary


-- Framework ------------------------------------------------------------------
main = defaultMain tests

tests
 =      [ testProperty "compute/chain          " prop_compute_chain
        , testProperty "compute/stream         " prop_compute_stream
        , testProperty "compute/toStream/chain " prop_compute_toStream_chain
        , testProperty "replicate              " prop_replicate
        , testProperty "replicates             " prop_replicates
        , testProperty "indexed/U              " prop_indexedU 
        , testProperty "indexed/N              " prop_indexedN
        , testProperty "pack                   " prop_pack
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
   in  vtoList  (vreplicate len' val)
    == replicate len' val


prop_replicates :: [Int] -> [Int] -> Bool
prop_replicates lens0 vec0
 = let  (lens, vec)     = P.unzip $ P.zip (P.map (`mod` maxRepl) lens0) vec0
        maxRepl         = 100

        lens'           = vfromListUnboxed lens
        idxs'           = vfromListUnboxed (scanl (+) 0 lens)
        vec'            = vfromListUnboxed vec
        !(I# n')        = P.sum lens
        segd            = Segd lens' idxs' n'

   in   toList (vreplicates segd vec')
    ==  P.concat (P.zipWith P.replicate lens vec)


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

