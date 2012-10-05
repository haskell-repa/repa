{-# LANGUAGE BangPatterns, ExistentialQuantification #-}

module Sequential (test) where
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Control.Monad.ST
import System.IO.Unsafe


data Step a s
        = Yield  a s
        | Update s 

data Stream a
        = forall s. Stream Int (s -> Step a s) s


vstream :: U.Unbox a => U.Vector a -> Stream a
vstream !vec
 = Stream (U.length vec) mkStep 0
 where  mkStep ix 
         = Yield (vec `U.unsafeIndex` ix) (ix + 1) 
        {-# INLINE [0] mkStep #-}
{-# INLINE [1] vstream #-}


vunstream :: U.Unbox a => Stream a -> U.Vector a
vunstream (Stream len mkStep s0)
 = runST
 $ do   mvec    <- UM.unsafeNew len
        fillLinear (UM.unsafeWrite mvec) mkStep s0
        U.unsafeFreeze mvec
{-# INLINE [1] vunstream #-}


fillLinear write mkStep s0
 = fill 0 s0
 where  fill ix s
         = case mkStep s of
                Yield x s'
                 -> do  write ix x
                        fill (ix + 1) s'

                Update s'
                 ->     fill (ix + 1) s'
        {-# INLINE [0] fill #-}
{-# INLINE [1] fillLinear #-}


smap :: (a -> b) -> Stream a -> Stream b
smap f (Stream len mkStep s0)
 = Stream len mkStep' s0
 where  mkStep' s
         = case mkStep s of
                Yield x s'      -> Yield (f x) s'
                Update s'       -> Update s'
        {-# INLINE [0] mkStep' #-}
{-# INLINE [1] smap #-}


test :: U.Vector Int -> U.Vector Int
test vec
        = vunstream $ smap (* 1234) $ vstream vec
