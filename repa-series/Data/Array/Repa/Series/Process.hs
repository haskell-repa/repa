
module Data.Array.Repa.Series.Process
        ( Process (..)
        , with, (%)
        , runProcess
        , runProcess2)
where
import Data.Array.Repa.Series.Series
import Data.Array.Repa.Series.Vector
import Data.Vector.Primitive                    (Prim)
import qualified Data.Vector.Primitive          as P
import qualified Data.Array.Repa.Series.Vector  as V
import GHC.Exts


-- | A series process just wraps an IO action.
--   We wrap IO to hide the associated type casts in the cor eprogram.
data Process
        = Process (IO ())


-- | Combine two processes.
with :: Process -> Process -> Process
with (Process a1) (Process a2)
 = Process (a1 >> a2)
{-# INLINE [1] with #-}


infixl %
(%) = with
{-# INLINE (%) #-}


-------------------------------------------------------------------------------
-- | Evaluate a series process, feeding it an unboxed vector.
--
--   The rate variable @k@ represents the length of the series.
runProcess
 :: Prim a
 => Vector a 
 -> (forall k. Series k a -> Process)  
                        -- ^ worker function
 -> IO ()

runProcess v1 f
 | l1  <- V.length v1
 = do   u1      <- V.toPrimitive v1
        let (Process go) = f (Series l1 u1)
        go
{-# INLINE [1] runProcess #-}


-- | Evaluate a series process,
--   feeding it two unboxed vectors of the same length.
runProcess2 
 ::               (Prim a,       Prim b)
 =>              Vector a   -> Vector b
 -> (forall k. Series k a -> Series k b -> Process)  
                        -- ^ worker function
 -> IO Bool

runProcess2 v1 v2 f
 | l1 <- V.length v1
 , l2 <- V.length v2, eqWord# l1 l2
 = do   u1      <- V.toPrimitive v1
        u2      <- V.toPrimitive v2
        let (Process go) 
                = f (Series l1 u1) (Series l2 u2)
        x       <- go
        return  True

 | otherwise
 = return False
{-# INLINE [1] runProcess2 #-}
