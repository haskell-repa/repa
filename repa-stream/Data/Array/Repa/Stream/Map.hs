
module Data.Array.Repa.Stream.Map
        (map,          mapD)
where
import Data.Array.Repa.Stream.Base
import Prelude  hiding (map)


-- map ------------------------------------------------------------------------
-- | Apply a function to every element of a stream.
map :: (a -> b) -> Stream a -> Stream b
map f (Stream len start next)
 = Stream len start next'
 where  next' s
         = case next s of
                Yield  s' x     -> Yield  s' (f x)
                Update s'       -> Update s'
                Done            -> Done
{-# INLINE [1] map #-}


-- | Apply a function to every element of a distributed stream.
mapD :: (a -> b) -> DistStream a -> DistStream b
mapD f (DistStream size frags frag)
 = DistStream size frags frag'
 where  frag' c' = map f (frag c')
{-# INLINE [1] mapD #-}
