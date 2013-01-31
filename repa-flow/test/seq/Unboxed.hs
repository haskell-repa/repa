{-# LANGUAGE MagicHash #-}
-- | Data.Vector.Unboxed harness for testing.
module Unboxed 
        ( uflow
        , szip
        , example1
        , module Data.Array.Repa.Flow.Seq)
where
import Data.Array.Repa.Bulk.Elt
import Data.Array.Repa.Flow.Seq
import Data.Array.Repa.Flow.Seq         as S
import qualified Data.Vector.Unboxed    as U
import Data.Vector.Unboxed              (Vector, Unbox)
import GHC.Exts

szip    = S.zip

uflow :: (Unbox a, Elt a) => Vector a -> Flow mode a
uflow vec
 = let  get ix          = U.unsafeIndex vec (I# ix)
        !(I# len)       = U.length vec
   in   S.flow get len
{-# INLINE uflow #-}


example1 :: Vector (Int, Int) -> IO (Int, Int)
example1 vec
 = do   (xs, ys)        <- S.unzip $ uflow vec
        
        let xs' = S.map (+ 1234) xs
        let ys' = S.map (* 2345) ys

        !x      <- S.foldl (+) 0 xs'
        !y      <- S.foldl (+) 0 ys'
        return (x, y)
