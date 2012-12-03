
module Solver (smvm) where
import Data.Array.Repa.Vector.Segd
import Data.Array.Repa.Vector.Repr.Unboxed
import Data.Array.Repa.Vector                   as R
import Prelude                                  as P

smvm    :: Segd 
        -> Vector U (Int, Double)
        -> Vector U Double
        -> Vector U Double

smvm !segd !matrix !vector
 = let  (!ixs, !vals)   = R.unzip matrix
   in   R.unflowP
         $ R.sums segd
         $ R.zipWith (*) vals
         $ R.gather1  vector ixs
{-# NOINLINE smvm #-}

