{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE ParallelListComp, MagicHash, BangPatterns, ScopedTypeVariables #-}

-- import Test.Framework
-- import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck                  
import Data.Array.Repa
import Data.Array.Repa.Vector
import Data.Array.Repa.Vector.Segd              (Segd(..))
import System.IO.Unsafe
import qualified Data.Vector.Unboxed            as U
import qualified Data.Array.Repa.Vector.Segd    as Segd
import Prelude                                  as P
import Data.List
import GHC.Exts 
import Control.Applicative ((<$>))

import Solver
--import System.IO.Unsafe
import Debug.Trace


-- Framework ------------------------------------------------------------------
main = quickCheck prop_smvm

data SparseMatrixVector = SparseMatrixVector [[(Int,Double)]] [Double]
    deriving (Show)

instance Arbitrary SparseMatrixVector where
 arbitrary
  = do  Positive sz     <- arbitrary
        let sz' = sz `mod` 500
        vec             <- mapM (const arbitrary) [0..sz']
        mtx             <- mapM (mapM getM) $ replicate sz' [0..sz']
        return $ SparseMatrixVector (P.map concat mtx) vec
  where
    getM i = do
        b <- arbitrary
        case b of
         True  -> arbitrary >>= \d -> return [(i,d)]
         False -> return []

prop_smvm :: SparseMatrixVector -> Bool
prop_smvm (SparseMatrixVector mtx vec)
 = let result =  vtoList $ unsafePerformIO $ smvm (segd mtx) (fromList' $ concat mtx) (fromList' vec)
       expect =  vtoList $ fromList'       $ smvm_list mtx vec
   in  trace (show result Data.List.++ " ==? " Data.List.++ show expect) result `eq_doubles` expect

eq_doubles :: [Double] -> [Double] -> Bool
eq_doubles xs ys
 | length xs == length ys = and (Data.List.zipWith eq_d xs ys)
 | otherwise              = False
 where
  eq_d x y = truncate (x*1000) == truncate (y*1000)

smvm_list m v = [ sum [ x * (v !! i) | (i,x) <- row]
                      | row <- m ]


fromList' :: U.Unbox a => [a] -> Vector U a
fromList' l = fromListUnboxed (Z :. length l) l

segd :: [[a]] -> Segd U U
segd l
 = let lens  = P.map length l
       lens' = fromList' lens
       ixs   = init $ scanl (+) 0 lens
       ixs'  = fromList' ixs
       !(I# el) = sum lens
   in  Segd lens' ixs' el

