{-# LANGUAGE BangPatterns, MagicHash #-}
module Solver
        (smvm)
where
import Data.Array.Repa                          as R
import Data.Array.Repa.Repr.Unboxed             as R
import Data.Array.Repa.Vector                   as R
import Prelude                                  as P
import Data.Array.Repa.Vector.Segd              (Segd(..))
import qualified Data.Array.Repa.Vector.Segd    as Segd
import GHC.Exts
import qualified Data.Array.Repa.Vector.Operators.Fold as R
import Data.Array.Repa.Repr.Stream
import qualified Data.Array.Repa.Vector.Operators.Indexs as I

{-# NOINLINE smvm #-}
smvm :: Segd U U -> Vector U (Int,Double)
     -> Vector U Double
     -> IO (Vector U Double)
smvm !segd !matrix !vector
 = do   let (!ixs,!vals)   = R.unzip matrix
        let  !vixs         = I.vindexs ixs vector
        let  !vals'        = vzipWith (*) vals vixs
        let  !res          = R.sum_s (Segd.splitSegd segd) vals'
        return res

