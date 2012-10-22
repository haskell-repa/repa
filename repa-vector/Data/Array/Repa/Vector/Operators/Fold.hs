{-# LANGUAGE BangPatterns, MagicHash #-}
module Data.Array.Repa.Vector.Operators.Fold
        ( fold_s
        , foldSegsWithP
        , sum_s
        , count_s
        , catDistStream)
where
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa.Vector.Segd
import Data.Array.Repa.Vector.Operators.Map
import Data.Array.Repa.Stream
import Data.Array.Repa                          as R
import Data.Array.Repa.Vector.Base
import qualified Data.Vector.Unboxed            as U
import GHC.Exts


-- | Segmented Fold
fold_s  :: (Source r a, Source r1 Int, Source r2 Int, U.Unbox a)
        => (a -> a -> a)
        -> a
        -> Segd r1 r2 -> Vector r a -> Vector U a
fold_s k z segd vec = foldSegsWithP k z segd (distOf $ vstream vec)
 where
    {-# INLINE distOf #-}
    distOf (AStream _ dist _) = dist
{-# INLINE fold_s #-}


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
 =  Stream sz (0, Nothing) step
 where
    {-# INLINE step #-}
    step (I# frag, Nothing)
     =  if frag ==# frag_max
        then Done
        else let !str = frag_get frag
             in  case str of
                Stream !_ !_ !_ -> Update (I# (frag +# 1#), Just str)

    step (!frag, Just (Stream !str_sz !s !str_step))
     =  case str_step s of
         Done        -> Update (frag, Nothing)
         Update !s'  -> Update (frag, Just (Stream str_sz s' str_step))
         Yield !s' a -> Yield  (frag, Just (Stream str_sz s' str_step)) a


-- | Segmented sum.
sum_s   :: (Source r1 Int, Source r2 Int, Source r3 a, U.Unbox a, Num a)
        => Segd r1 r2 -> Vector r3 a -> Vector U a
sum_s segd vec
        = fold_s (+) 0 segd vec
{-# INLINE sum_s #-}


-- | Segmented count.
count_s :: ( Source r1 Int, Source r2 Int, Source r3 a, U.Unbox a
           , Source (MapR r3) Int
           , Map r3 a
           , Eq a)
        => Segd r1 r2 -> Vector r3 a -> a -> Vector U Int

count_s segd vec x
 = sum_s segd $ vmap (fromBool . (== x)) vec
 where  fromBool True  = 1
        fromBool False = 0
