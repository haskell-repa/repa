{-# LANGUAGE BangPatterns, MagicHash #-}
module Solver 
        (smvm)
where
import Data.Vector.Unboxed
import Data.Array.Repa.Flow.Par                 (Flow)
import Data.Array.Repa.Flow.Par.Segd            (Segd)
import qualified Data.Array.Repa.Flow.Par       as F
import qualified Data.Vector.Unboxed            as U
import GHC.Exts

smvm    :: Segd                 -- ^ Segment descriptor for matrix.
        -> Vector (Int, Double) -- ^ Sparse matrix column number and coefficient.
        -> Vector Double        -- ^ Dense vector.
        -> Vector Double

smvm !segd !vMatrix !vVector
 = let  (vColId, vColVal)   = U.unzip vMatrix
   in   F.unflow
          $ F.sums   segd
          $ F.zipLeftWith (*)
                (F.gather vVector (F.flow vColId))
                (\ix -> U.unsafeIndex vColVal (I# ix))
