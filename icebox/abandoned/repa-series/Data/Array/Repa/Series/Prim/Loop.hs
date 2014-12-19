
module Data.Array.Repa.Series.Prim.Loop where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Prim.Utils
import Data.Array.Repa.Series.Prim.Word
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Ref       as Ref
import GHC.Exts
import GHC.Types
import Debug.Trace


-- Loop combinators -----------------------------------------------------------
-- | Primitive stateful loop combinator.
repa_loop       :: Word# -> (Word# -> W -> W) -> W -> W
repa_loop len worker world0
 = go (int2Word# 0#) world0
 where  
        go ix world
         | 1# <- geWord# ix len           
         = world

         | world' <- worker ix world
         = go (plusWord# ix (int2Word# 1#)) world'
{-# INLINE [1] repa_loop #-}


-- | Guard an inner context with a flag.
repa_guard      :: Ref Word -> Bool -> (Word# -> W -> W) -> W -> W
repa_guard ref flag worker w0
 | False        <- flag
 = w0

 | (# w1, ix #) <- repa_readRefWord  ref w0
 , w2           <- repa_writeRefWord ref (plusWord# ix (int2Word# 1#)) w1
 , w3           <- worker ix w2
 = w3
{-# INLINE [1] repa_guard #-}


-- | Process 4-element chunks with one function, and the rest with some other.
repa_split4     
        :: forall k
        .  RateNat k 
        -> (RateNat (Down4 k) -> W -> W)
        -> (RateNat (Tail4 k) -> W -> W)
        -> W -> W

repa_split4 (RateNat len) goDown4 goTail4 w0
 | chunks       <- len `quotWord#` (int2Word# 4#)
 , leftover     <- len `remWord#`  (int2Word# 4#)
 , w1           <- goDown4 (RateNat chunks) w0
 , w2           <- goTail4 (RateNat leftover) w1
 = w2
{-# INLINE [1] repa_split4 #-}


-- | Process 8-element chunks with one function, and the rest with some other.
repa_split8     
        :: forall k
        .  RateNat k 
        -> (RateNat (Down8 k) -> W -> W)
        -> (RateNat (Tail8 k) -> W -> W)
        -> W -> W

repa_split8 (RateNat len) goDown8 goTail8 w0
 | chunks       <- len `quotWord#` (int2Word# 8#)
 , leftover     <- len `remWord#`  (int2Word# 8#)
 , w1           <- goDown8 (RateNat chunks) w0
 , w2           <- goTail8 (RateNat leftover) w1
 = w2
{-# INLINE [1] repa_split8 #-}

