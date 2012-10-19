
-- TODO: 
--  If we had a better way of encoding balancedness in the Dist type
--  then we could turn these into Chain functions instead.
--
--  The trouble is that if we duplicate each element of a balanced vector
--  then the result is only weakly balanced. We know how many element's we'll
--  get, and we know that the number of elements between two threads can differ
--  by a small statically known amount.
--
--  Eg start with:  
--         | . . . | . . . | . . | . . |
--
--  apply flatten3:
--     =>  | . .  . .  . . |  . .  . .  . . | . .  . . | . .  . . |
--
--  Now thread 3 contains two less elements than thread 1, 
--  instead of 1 less element.
--
--  Maybe we can encode the degree of balancedness in the Chain type.
--    eg Chain B1 Int, vs Chain B2 Int.
--
--  We can still zip two B2 Chains, because pairs of threads are guaranteed
--  to have the same number of elements.
--

module Data.Array.Repa.Stream.Flatten
        ( flatten2,     flatten2D
        , flatten3,     flatten3D
        , flatten4,     flatten4D)
where
import Data.Array.Repa.Stream.Base


-- Flatten2 -------------------------------------------------------------------
-- | Flatten a stream of tuples in to a stream of their elements.
flatten2 :: Stream (a, a) -> Stream a
flatten2 (Stream size start next)
 = Stream (scaleSize size 2#) (start, Nothing) next'
 where  
        next' (s, Nothing)
         = case next s of
                Yield  s' xs'@(x1, _) -> Yield  (s', Just xs') x1
                Update s'             -> Update (s', Nothing)
                Done                  -> Done

        next' (s, Just (_, x2))
         = Yield (s, Nothing) x2
{-# INLINE [1] flatten2 #-}


-- | Flatten a distributed stream of tuples into a distributed stream of elements.
flatten2D :: DistStream (a, a) -> DistStream a
flatten2D (DistStream size frags frag)
 = DistStream (scaleSize size 2#) frags frag'
 where  frag' c     = flatten2 (frag c)
{-# INLINE [1] flatten2D #-}


-- Flatten3 -------------------------------------------------------------------
-- | Flatten a stream of 3-tuples in to a stream of their elements.
flatten3 :: Stream (a, a, a) -> Stream a
flatten3 (Stream size start next)
 = Stream (scaleSize size 3#) (start, 0 :: Int, Nothing) next'
 where  
        next' (s, n, Nothing)
         = case next s of
                Yield  s' xs'@(x0, _, _) 
                 -> Yield  (s', 1, Just xs') x0

                Update s' -> Update (s', n, Nothing)
                Done      -> Done

        next' (s, 1, Just xs@(_, x1, _))
         = Yield (s, 2, Just xs) x1

        next' (s, _, Just    (_, _, x2))
         = Yield (s, 0, Nothing) x2
{-# INLINE [1] flatten3 #-}


-- | Flatten a distributed stream of tuples into a distributed stream of elements.
flatten3D :: DistStream (a, a, a) -> DistStream a
flatten3D (DistStream size frags frag)
 = DistStream (scaleSize size 3#) frags frag'
 where  frag' c     = flatten3 (frag c)
{-# INLINE [1] flatten3D #-}


-- Flatten4 -------------------------------------------------------------------
-- | Flatten a stream of 4-tuples in to a stream of their elements.
flatten4 :: Stream (a, a, a, a) -> Stream a
flatten4 (Stream size start next)
 = Stream (scaleSize size 4#) (start, 0 :: Int, Nothing) next'
 where  
        next' (s, n, Nothing)
         = case next s of
                Yield  s' xs'@(x0, _, _, _) 
                 -> Yield  (s', 1, Just xs') x0

                Update s' -> Update (s', n, Nothing)
                Done      -> Done

        next'    (s, 1, Just xs@(_, x1, _, _))
         = Yield (s, 2, Just xs) x1

        next'    (s, 2, Just xs@(_, _, x2, _))
         = Yield (s, 3, Just xs) x2

        next'    (s, _, Just    (_, _, _, x3))
         = Yield (s, 0, Nothing) x3
{-# INLINE [1] flatten4 #-}


-- | Flatten a distributed stream of 4-tuples into a distributed stream of elements.
flatten4D :: DistStream (a, a, a, a) -> DistStream a
flatten4D (DistStream size frags frag)
 = DistStream (scaleSize size 4#) frags frag'
 where  frag' c     = flatten4 (frag c)
{-# INLINE [1] flatten4D #-}
