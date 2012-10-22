

module Data.Array.Repa.Stream.Scan
        (mapAccum)
where
import Data.Array.Repa.Stream.Base


mapAccum :: (acc -> a -> (acc,b)) 
          -> acc -> Stream a -> Stream b

mapAccum f acc0 (Stream size state0 next)
 = Stream size (acc0, state0) next'
 where  next' (acc, s) 
         = case next s of
            Yield s' x  -> let (acc', y) = f acc x
                           in  Yield (acc', s') y

            Update  s'  -> Update (acc, s')
            Done        -> Done

{-# INLINE mapAccum #-}
