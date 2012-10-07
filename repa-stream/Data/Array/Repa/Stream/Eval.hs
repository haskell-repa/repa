
module Data.Array.Repa.Stream.Eval
        (evalM, evalMD)
where
import Data.Array.Repa.Stream.Base
import GHC.Exts


-- | Evaluate a stream,
--   calling the given function with the index and value of each element.
evalM   :: Monad m
        => (Int# -> a -> m ())
        -> Stream a
        -> m ()

evalM write (Stream _size s0 next)
 = go 0# s0
 where  go ix !s
         = case next s of
                Yield !s' x
                 -> do  write ix x
                        go (ix +# 1#) s'

                Update !s'
                 ->     go ix s'

                Done
                 ->     return ()
{-# INLINE [1] evalM #-}


-- | Evaluate a distributed stream,
--   calling the given function with the fragment index, 
--   and element index within each fragment.
evalMD  :: Monad m
        => (Int# -> Int# -> a -> m ())
        -> DistStream a
        -> m ()

evalMD write (DistStream _size frags frag)
 = go 0#
 where  go i
         | i >=# frags
         = return ()

         | otherwise
         = do   evalM (write i) (frag i)
                go (i +# 1#)
{-# INLINE [1] evalMD #-}
