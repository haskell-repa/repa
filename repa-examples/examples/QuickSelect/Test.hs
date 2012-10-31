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
main = quickCheck prop_qs

prop_qs :: Int -> [Int] -> Int -> Bool
prop_qs x1 xs1 k
 = let xs  = (x1:xs1) -- non-empty
       k'  = k `mod` length xs
       xs' = fromList' xs

       expect = (sort xs) !! k'
       result = quick_select xs' (unbox k')
   in  result == expect

unbox :: Int -> Int#
unbox (I# i) = i

fromList' :: U.Unbox a => [a] -> Vector U a
fromList' l = fromListUnboxed (Z :. length l) l

