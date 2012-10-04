
module Data.Array.Repa.Chain.Map
        (map,   mapD)
where
import Data.Array.Repa.Chain.Base
import Prelude  hiding (map)


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
