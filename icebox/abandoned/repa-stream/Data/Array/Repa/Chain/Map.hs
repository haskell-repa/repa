
module Data.Array.Repa.Chain.Map
        ( map,           mapD
        , mapIx,         mapIxD
        , unsafeZipLock, unsafeZipLockD)
where
import Data.Array.Repa.Chain.Base
import Prelude  hiding (map, zipWith, zipWith3)
import GHC.Exts

-- map ------------------------------------------------------------------------
-- | Apply a function to every element of a chain.
map :: (a -> b) -> Chain a -> Chain b
map f (Chain len start next)
 = Chain len start next'
 where  next' ix s
         = case next ix s of
                Yield  s' x     -> Yield  s' (f x)
                Update s'       -> Update s'
{-# INLINE [1] map #-}


-- | Apply a function to every element of a distributed chain.
mapD :: (a -> b) -> DistChain a -> DistChain b
mapD f (DistChain distro frag)
 = DistChain distro frag'
 where  frag' c' = map f (frag c')
{-# INLINE [1] mapD #-}


-- mapIx ----------------------------------------------------------------------
-- | Apply a function to every element of a chain.
mapIx :: (Int# -> a -> b) -> Chain a -> Chain b
mapIx f (Chain len s0 next)
 = Chain len s0 next'
 where  
        next' ix s
         = case next ix s of
                Yield s' x      -> Yield s' (f ix x)
                Update s'       -> Update s'
{-# INLINE [1] mapIx #-}


-- | Apply a function to every element of a distributed chain.
mapIxD :: (Int# -> a -> b) -> DistChain a -> DistChain b
mapIxD f (DistChain distro frag)
 = DistChain distro frag'
 where  frag' i = mapIx f (frag i)


-- zipLock --------------------------------------------------------------------
-- | Combine two chains that have the same length.
--   If they don't have the same length then undefined.
unsafeZipLock :: (a -> b -> c) -> Chain a -> Chain b -> Chain c
unsafeZipLock f (Chain len1 start1 next1)
          (Chain _    start2 next2)
 = Chain len1 (T2 start1 start2) next'
 where  
        next' ix (T2 s1 s2)
         = case (next1 ix s1, next2 ix s2) of
                (Yield  s1' x1, Yield s2' x2)   -> Yield  (T2 s1' s2') (f x1 x2)
                (Update s1',    _)              -> Update (T2 s1' s2)
                (_,             Update s2')     -> Update (T2 s1  s2')
{-# INLINE [1] unsafeZipLock #-}


-- | Combine two distributed chains that have the same distribution. 
--   If they don't have the same distribution then undefined.
unsafeZipLockD :: (a -> b -> c) -> DistChain a -> DistChain b -> DistChain c
unsafeZipLockD f (DistChain distro1 frag1) (DistChain _ frag2)
 = DistChain distro1 frag'
 where  frag' i = unsafeZipLock f (frag1 i) (frag2 i)
{-# INLINE [1] unsafeZipLockD #-}

data T2 a b
        = T2 !a !b
