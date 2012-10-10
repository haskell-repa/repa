
module Data.Array.Repa.Stream.Map
        ( map,          mapD
        , zipWith,      zipWithD)
where
import Data.Array.Repa.Stream.Base
import Prelude  hiding (map, zipWith)


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


-- zipWith --------------------------------------------------------------------
data T2 a b
        = T2 !a !b

-- | Combine two streams with the same length.
--   If they don't have the same length then undefined.
zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (Stream len1 start1 next1)
          (Stream _    start2 next2)
 = Stream len1 (T2 start1 start2) next'
 where  
        next' (T2 s1 s2)
         = case (next1 s1, next2 s2) of
                (Yield  s1' x1, Yield s2' x2)   -> Yield  (T2 s1' s2') (f x1 x2)
                (Update s1',    _)              -> Update (T2 s1' s2)
                (_,             Update s2')     -> Update (T2 s1  s2')
                (Done,          _)              -> Done
                (_,             Done)           -> Done
{-# INLINE [1] zipWith #-}


-- | Combine two streams that have the same distribution. 
--   If they don't have the same distribution then undefined.
zipWithD :: (a -> b -> c) -> DistStream a -> DistStream b -> DistStream c
zipWithD f (DistStream size frags frag1) (DistStream _ _ frag2)
 = DistStream size frags frag'
 where  frag' i = zipWith f (frag1 i) (frag2 i)
{-# INLINE [1] zipWithD #-}
