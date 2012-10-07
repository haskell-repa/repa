
module Data.Array.Repa.Stream.Pack
        (pack)
where
import Data.Array.Repa.Stream.Base


-- | Given a stream of flags and values, return a stream of values that had
--   their correspondig flag set to `True`.
pack :: Stream (Bool, a) -> Stream a
pack (Stream size start next)
 = Stream size' start next'
 where  
        size'
         = case size of
                Unknown         -> Unknown
                Max   len       -> Max len
                Exact len       -> Max len

        next' s
         = case next s of
                Yield s' (flag, x) 
                 | flag         -> Yield  s' x
                 | otherwise    -> Update s'

                Update s'       -> Update s'
                Done            -> Done
{-# INLINE [1] pack #-}
