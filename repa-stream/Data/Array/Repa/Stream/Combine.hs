
module Data.Array.Repa.Stream.Combine
        ( combine2
        , combineSegs2 )
where
import Data.Array.Repa.Stream.Base
import Prelude  hiding (map, zipWith)


-- | Combine two streams, using a tag stream to tell us which of the data
--   streams to take the next element from.
--
--   If there are insufficient elements in the data streams for the provided
--   tag stream then `error`.
--  
-- @
-- combine2 [F T T F F T] [1 2 3] [4 5 6]
--  = [1 4 5 2 3 6]
-- @
--
combine2 :: Stream Bool -> Stream a -> Stream a -> Stream a
combine2 (Stream size sT0 nextT) 
         (Stream _    sA0 nextA) (Stream _ sB0 nextB)

 = Stream size (Nothing, sT0, sA0, sB0) next
 where
        next (Nothing, sT, sA, sB)
         = case nextT sT of
            Yield  s' t  -> Update (Just t,  s', sA, sB)
            Update s'    -> Update (Nothing, s', sA, sB)
            Done         -> Done

        next (Just False, sT, sA, sB)
         = case nextA sA of
            Yield  sA' x -> Yield  (Nothing,    sT, sA', sB) x
            Update sA'   -> Update (Just False,  sT, sA', sB)
            Done         -> error "combine2ByTagS: stream 1 too short"

        next (Just True, sT, sA, sB)
         = case nextB sB of
            Yield  sB' x -> Yield  (Nothing,    sT, sA, sB') x
            Update sB'   -> Update (Just True, sT, sA, sB')
            Done         -> error "combine2ByTagS: stream 2 too short"

{-# INLINE [1] combine2 #-}


-- | Segmented Stream combine. Like `combine2ByTag`, except that the tags select
--   entire segments of each data stream, instead of selecting one element at a time.
--
-- @
-- combineSegs2 
--      [F, F, T, F, T, T]
--      [2,1,3] [10,20,30,40,50,60]
--      [1,2,3] [11,22,33,44,55,66]
--  = [10,20,30,11,40,50,60,22,33,44,55,66]
-- @
--
--   This says take two elements from the first stream, then another one element 
--   from the first stream, then one element from the second stream, then three
--   elements from the first stream...
--
combineSegs2
        :: Stream Bool          -- ^ tag values
        -> Stream Int           -- ^ segment lengths for first data stream
        -> Stream a             -- ^ first data stream
        -> Stream Int           -- ^ segment lengths for second data stream
        -> Stream a             -- ^ second data stream
        -> Stream a

combineSegs2 
        (Stream _ sf0  nextf) 
        (Stream _ ss10 nexts1) (Stream nv1 vs10 nextv1)
        (Stream _ ss20 nexts2) (Stream nv2 vs20 nextv2)

 = Stream (nv1 `addSize` nv2)
          (Nothing, True, sf0, ss10, vs10, ss20, vs20)
          next

 where  next (Nothing, f, sf, ss1, vs1, ss2, vs2) 
         = case nextf sf of
            Done                -> Done
            Update sf'          -> Update (Nothing, f, sf', ss1, vs1, ss2, vs2) 
            Yield  sf' False
             -> case nexts1 ss1 of
                 Done           -> Done
                 Update ss1'    -> Update (Nothing, f,     sf, ss1', vs1, ss2, vs2) 
                 Yield  ss1' n  -> Update (Just n,  False, sf',ss1', vs1, ss2, vs2) 

            Yield  sf' True
             -> case nexts2 ss2 of
                 Done           -> Done
                 Update ss2'    -> Update (Nothing, f,     sf, ss1, vs1, ss2', vs2) 
                 Yield  ss2' n  -> Update (Just n,  True,  sf',ss1, vs1, ss2', vs2)

        next (Just 0, _, sf, ss1, vs1, ss2, vs2) 
         = Update (Nothing, True, sf, ss1, vs1, ss2, vs2)

        next (Just n, False, sf, ss1, vs1, ss2, vs2) 
         = case nextv1 vs1 of
            Done                -> Done
            Update vs1'         -> Update (Just n,     False, sf, ss1, vs1', ss2, vs2) 
            Yield  vs1' x       -> Yield  (Just (n-1), False, sf, ss1, vs1', ss2, vs2) x

        next (Just n, True, sf, ss1, vs1, ss2, vs2) 
         = case nextv2 vs2 of
            Done                -> Done
            Update vs2'         -> Update (Just n,     True, sf, ss1, vs1, ss2, vs2') 
            Yield  vs2' x       -> Yield  (Just (n-1), True, sf, ss1, vs1, ss2, vs2') x
{-# INLINE [1] combineSegs2 #-}

