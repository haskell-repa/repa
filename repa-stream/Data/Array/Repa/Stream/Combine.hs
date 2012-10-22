
module Data.Array.Repa.Stream.Combine
        (combine2ByTag)
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
-- combine2ByTag [0 1 1 0 0 1] [1 2 3] [4 5 6]
--  = [1 4 5 2 3 6]
-- @
--
combine2ByTag :: Stream Int -> Stream a -> Stream a -> Stream a
combine2ByTag (Stream size sT0 nextT) 
              (Stream _    sA0 nextA) (Stream _ sB0 nextB)

  = Stream size (Nothing, sT0, sA0, sB0) next
  where 
        next (Nothing, sT, sA, sB)
         = case nextT sT of
            Yield  s' t  -> Update (Just t,  s', sA, sB)
            Update s'    -> Update (Nothing, s', sA, sB)
            Done         -> Done

        next (Just 0, sT, sA, sB)
         = case nextA sA of
            Yield  sA' x -> Yield  (Nothing, sT, sA', sB) x
            Update sA'   -> Update (Just 0,  sT, sA', sB)
            Done         -> error "combine2ByTagS: stream 1 too short"

        next (Just t, sT, sA, sB)
         = case nextB sB of
            Yield  sB' x -> Yield  (Nothing, sT, sA, sB') x
            Update sB'   -> Update (Just t,  sT, sA, sB')
            Done         -> error "combine2ByTagS: stream 2 too short"

{-# INLINE [1] combine2ByTag #-}
