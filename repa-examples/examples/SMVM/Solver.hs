{-# LANGUAGE BangPatterns, MagicHash #-}
module Solver
        (smvm)
where
import Data.Array.Repa.Vector                   as R
import Prelude                                  as P


smvm    :: Segd 
        -> Vector U (Int, Double)
        -> Vector U Double
        -> IO (Vector U Double)

smvm !segd !matrix !vector
 = do   let (!ixs,!vals)   = R.unzip matrix
        let  !vixs         = I.vindexs ixs vector
        let  !vals'        = vzipWith (*) vals vixs
        let  !res          = R.sum_s (Segd.splitSegd segd) vals'
        return res
{-# NOINLINE smvm #-}

