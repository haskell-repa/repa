{-# LANGUAGE BangPatterns, MagicHash #-}
module Data.Array.Repa.Vector.Operators.Fold
    ( fold_s
    , foldSegsWithP)
where
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa.Vector.Segd
import Data.Array.Repa.Stream

import Data.Array.Repa                          as R
import Data.Array.Repa.Vector.Base

import qualified Data.Vector.Unboxed            as U

import GHC.Exts

{-# INLINE fold_s #-}
fold_s  :: (Source r a, Source r1 Int, Source r2 Int, U.Unbox a)
        => (a -> a -> a)
        -> a
        -> Segd r1 r2 -> Vector r a -> Vector U a
fold_s k z segd vec = foldSegsWithP k z segd (distOf $ vstream vec)
 where
    {-# INLINE distOf #-}
    distOf (AStream _ dist _) = dist

{-# INLINE foldSegsWithP #-}
foldSegsWithP
        :: (Source S a, Source r1 Int, Source r2 Int, U.Unbox a)
        => (a -> a -> a)
        -> a
--        -> (Stream Int -> Stream a -> Stream a)
        -> Segd r1 r2 -> DistStream a -> Vector U a

foldSegsWithP fElem zero segd xss
 = unstream $ foldSegs fElem zero (instream $ lengths segd) (catDistStream xss)
 where
    {-# INLINE unstream #-}
    unstream s =
        let u = unstreamUnboxed s
        in  AUnboxed (Z :. U.length u) u

    {-# INLINE instream #-}
    instream v =
        stream (unbox (vlength v)) (\ix -> R.unsafeLinearIndex v (I# ix))

    {-# INLINE unbox #-}
    unbox (I# int) = int

{-# INLINE catDistStream #-}
catDistStream :: DistStream a -> Stream a
catDistStream (DistStream sz frag_max frag_get)
 = Stream sz (0, Nothing) step
 where
    {-# INLINE step #-}
    step (I# frag, Nothing)
     =  if frag ==# frag_max
        then Done
        else Update (I# (frag +# 1#), Just $ frag_get frag)
    step (frag, Just (Stream str_sz s str_step))
     =  case str_step s of
         Done       -> Update (frag, Nothing)
         Update s'  -> Update (frag, Just (Stream str_sz s' str_step))
         Yield s' a -> Yield  (frag, Just (Stream str_sz s' str_step)) a


