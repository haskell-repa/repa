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

{-# NOINLINE smvm #-}
smvm :: Segd U U -> Vector U (Int,Double)
	 -> Vector U Double
	 -> IO (Vector U Double)
smvm segd matrix vector
 = do   let (!ixs,!vals)    = R.unzip matrix
        smvm_2 segd ixs vals vector

{-# NOINLINE smvm_2 #-}
smvm_2 segd ixs vals vector
 = let  vixs    = vmap (\i -> R.unsafeIndex vector (R.Z R.:. i)) ixs
        vals'   = vzipWith (*) vals vixs
        res     = R.fold_s (+) 0 segd vals'
   in   return res
