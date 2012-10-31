{-# LANGUAGE BangPatterns, MagicHash #-}
module Solver
        (quick_select)
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

{-# NOINLINE quick_select #-}
quick_select
    -- I suspect using (Ord a => Vector U a) would produce worse core
    :: Vector U Int
    -> Int#
    -> Int
quick_select !s !k
 = let  !p  = vindex s         $ vlength s `div` 2
        !l  = vcomputeUnboxedP $ vfilter (<p) s
   in
        if      (I# k) < vlength l
        then    quick_select l k
        else
            let !g        = vcomputeUnboxedP $ vfilter (>p) s
                !(I# len) = vlength s - vlength g
            in  
                if   k >=# len
                then quick_select g (k -# len)
                else p

