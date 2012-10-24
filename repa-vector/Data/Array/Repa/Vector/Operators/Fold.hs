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
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import GHC.Exts
import Control.Monad
import Control.Monad.ST


-- | Segmented Fold
fold_s  :: (Source r a, U.Unbox a)
        => (a -> a -> a)
        -> a
        -> SplitSegd -> Vector r a -> Vector U a
fold_s k z segd vec = foldSegsWithP k (foldSegs k z) segd (distOf $ vstream vec)
 where
  distOf (AStream _ dist _) = dist
{-# INLINE fold_s #-}


{-# INLINE foldSegsWithP #-}
foldSegsWithP
        :: (Source S a, U.Unbox a)
        => (a -> a -> a)
        -> (Stream Int -> Stream a -> Stream a)
        -> SplitSegd -> DistStream a -> Vector U a
foldSegsWithP fElem fSeg segd (DistStream _sz num_frags get_frag)
 = runST (do
        mrs <- joinD drs -- V.convert drs
        --mrs <- joinDM theGang drs
        fixupFold fElem mrs dcarry
        liftM vfromUnboxed $ U.unsafeFreeze mrs)
 where
    (dcarry, drs)
        = V.unzip
        $ V.generate (I# num_frags) partial
        -- Note: assuming that (num_frags == splitChunks segd)

    joinD d = U.unsafeThaw
            $ V.convert
            $ V.concatMap V.convert d

    partial (I# i)
        = let chunk = splitChunk segd V.! I# i
              strm  = get_frag i

              rs    = fSeg (instream $ chunkLengths chunk) strm

              n | chunkOffset chunk ==# 0#  = 0
                | otherwise                 = 1
          in  ((I# (chunkStart chunk), U.take n $ unstreamUnboxed rs), U.drop n $ unstreamUnboxed rs)

    {-# INLINE instream #-}
    instream v =
        stream (unbox (vlength v))
               (\ix -> R.unsafeLinearIndex v (I# ix))

    {-# INLINE unbox #-}
    unbox (I# int) = int

    vfromUnboxed vec
        = fromUnboxed (Z :. U.length vec) vec

fixupFold
        :: (U.Unbox a)
        => (a -> a -> a)
        -> UM.MVector s a
        -> V.Vector (Int, U.Vector a)
        -> ST s ()
fixupFold f !mrs !dcarry = go 1
  where
    !p = V.length dcarry

    go i | i >= p = return ()
         | U.null c = go (i+1)
         | otherwise   = do
                           x <- UM.read mrs k
                           UM.write mrs k (f x (c U.! 0))
                           go (i + 1)
      where
        (k,c) = dcarry V.! i
{-# NOINLINE fixupFold #-}


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
sum_s   :: (Source r3 a, U.Unbox a, Num a)
        => SplitSegd -> Vector r3 a -> Vector U a
sum_s segd vec
        = fold_s (+) 0 segd vec
{-# INLINE sum_s #-}


-- | Segmented count.
count_s :: ( Source r3 a, U.Unbox a
           , Source (MapR r3) Int
           , Map r3 a
           , Eq a)
        => SplitSegd -> Vector r3 a -> a -> Vector U Int

count_s segd vec x
 = sum_s segd $ vmap (fromBool . (== x)) vec
 where  fromBool True  = 1
        fromBool False = 0

