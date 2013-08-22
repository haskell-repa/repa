
module Data.Array.Repa.Series.Prim.Loop where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Prim.Utils
import Data.Array.Repa.Series.Prim.Word
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Ref       as Ref
import GHC.Exts
import GHC.Types


-- Loop combinators -----------------------------------------------------------
-- | Primitive stateful loop combinator.
repa_loop
        :: Word#  
        -> (Word# -> World -> World) -> World -> World

repa_loop len worker world0
 = go (int2Word# 0#) world0
 where  
        go ix world
         | geWord# ix len           
         = world

         | world' <- worker ix world
         = go (plusWord# ix (int2Word# 1#)) world'
{-# INLINE repa_loop #-}


-- | Guard an inner context with a flag.
repa_guard
        :: Ref Word -> Bool
        -> (Word# -> World -> World)
        -> World -> World

repa_guard ref flag worker world0
 | False        <- flag
 = world0

 | (# world1, ix #) <- repa_readRefWord  ref world0
 , world2       <- repa_writeRefWord ref (plusWord# ix (int2Word# 1#)) world1
 , world3       <- worker ix world2
 = world3
{-# INLINE repa_guard #-}


repa_split4  
        :: forall k
        .  RateNat k 
        -> (RateNat (Down4 k) -> World -> World)
        -> (RateNat (Tail4 k) -> World -> World)
        -> World -> World
repa_split4 r fDown4 fTail4 w
 = error "repa_split4: not done yet"
{-# NOINLINE repa_split4 #-}

