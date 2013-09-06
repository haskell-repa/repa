
module Data.Array.Repa.Series.Process
        ( Process (..)
        , makeProcess
        , pjoin, (%)
        , runProcess
        , runProcess2
        , runProcess3
        , runProcess4)
where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Series
import Data.Array.Repa.Series.Vector
import Data.Array.Repa.Series.Prim.Utils
import Data.Vector.Primitive                    (Prim)
import qualified Data.Vector.Primitive          as P
import qualified Data.Array.Repa.Series.Vector  as V
import GHC.Exts


-- | A series process just wraps an IO action.
--   We wrap IO to hide the associated type casts in the cor eprogram.
data Process
        = Process (IO ())


-- | Wrap a World to World function into a process.
makeProcess :: (World -> World) -> Process
makeProcess f
 = Process $ wrapIO1 (\w -> (# f w, () #))
{-# INLINE [1] makeProcess #-}


-- | Combine two processes.
pjoin :: Process -> Process -> Process
pjoin (Process a1) (Process a2)
 = Process (a1 >> a2)
{-# INLINE [1] pjoin #-}


infixl %
(%) = pjoin
{-# INLINE (%) #-}
--  INLINE here needs to happen before lowering to expose the pjoin name.


-------------------------------------------------------------------------------
-- | Evaluate a series process, feeding it an unboxed vector.
--
--   The rate variable @k@ represents the length of the series.
runProcess
 :: Prim a
 => Vector a 
 -> (forall k. RateNat k -> Series k a -> Process)  
                        -- ^ worker function
 -> IO ()

runProcess v1 f
 | l1  <- V.length v1
 = do   s1      <- unsafeFromVector v1
        let rn          = RateNat l1
            Process go  = f rn s1
        go
{-# INLINE [1] runProcess #-}


-- | Evaluate a series process,
--   feeding it two unboxed vectors of the same length.
runProcess2 
 ::               (Prim a,       Prim b)
 =>              Vector a   -> Vector b
 -> (forall k. RateNat k -> Series k a -> Series k b -> Process)  
                        -- ^ worker function
 -> IO Bool

runProcess2 v1 v2 f
 | l1 <- V.length v1
 , l2 <- V.length v2, eqWord# l1 l2
 = do   s1      <- unsafeFromVector v1
        s2      <- unsafeFromVector v2
        let rn          = RateNat l1
            Process go  = f rn s1 s2
        x       <- go
        return  True

 | otherwise
 = return False
{-# INLINE [1] runProcess2 #-}


-- | Evaluate a series process,
--   feeding it three unboxed vectors of the same length.
runProcess3
 ::               (Prim a,       Prim b,     Prim c)
 =>              Vector a   -> Vector b   -> Vector c
 -> (forall k. RateNat k -> Series k a -> Series k b -> Series k c -> Process)  
                        -- ^ worker function
 -> IO Bool

runProcess3 v1 v2 v3 f
 | l1 <- V.length v1
 , l2 <- V.length v2, eqWord# l1 l2
 , l3 <- V.length v3, eqWord# l2 l3
 = do   s1      <- unsafeFromVector v1
        s2      <- unsafeFromVector v2
        s3      <- unsafeFromVector v3
        let rn          = RateNat l1
            Process go  = f rn s1 s2 s3
        x       <- go
        return  True

 | otherwise
 = return False
{-# INLINE [1] runProcess3 #-}


-- | Evaluate a series process,
--   feeding it three unboxed vectors of the same length.
runProcess4
 ::           (Prim a,       Prim b,     Prim c,  Prim d)
 =>          Vector a   -> Vector b   -> Vector c   -> Vector d
 -> (forall k. RateNat k 
            -> Series k a -> Series k b -> Series k c -> Series k d -> Process)  
                        -- ^ worker function
 -> IO Bool

runProcess4 v1 v2 v3 v4 f
 | l1 <- V.length v1
 , l2 <- V.length v2, eqWord# l1 l2
 , l3 <- V.length v3, eqWord# l2 l3
 , l4 <- V.length v4, eqWord# l3 l4
 = do   s1      <- unsafeFromVector v1
        s2      <- unsafeFromVector v2
        s3      <- unsafeFromVector v3
        s4      <- unsafeFromVector v4
        let rn          = RateNat l1
            Process go  = f rn s1 s2 s3 s4
        x       <- go
        return  True

 | otherwise
 = return False
{-# INLINE [1] runProcess4 #-}


