
module Data.Array.Repa.Chain.Eval
        (evalM,        evalMD)
where
import Data.Array.Repa.Chain.Base
import GHC.Exts


-- | Evaluate a chain, 
--   calling the given function with the index and value of each element.
evalM   :: Monad m 
        => (Int# -> a -> m ())
        -> Chain a
        -> m ()

evalM write c
 = evalM' 0# write c
{-# INLINE [1] evalM #-}


-- | Evaluate a distributed chain, 
--   calling the given function with the index and value of each element.
evalMD  :: Monad m
        => (Int# -> a -> m ())
        -> DistChain a
        -> m ()

evalMD write (DistChain d frag)
 = go 0#
 where  go i
         | i >=# distroFrags d
         = return ()

         | otherwise
         = do   evalM' (distroFragStart d i) write (frag i)
                go (i +# 1#)
{-# INLINE [1] evalMD #-}



-- Evaluate a chain, 
-- calling the given function with the index and value of each element.
-- 
-- NOTE: We don't export this function because only Chains defined as
--       fragments of DistChains are guaranteed to be able to start 
--       from a non-zero result index.
--
evalM'  :: Monad m 
        => Int#
        -> (Int# -> a -> m ())
        -> Chain a
        -> m ()

evalM' ix0 write (Chain len s0 next)
 = go ix0 s0

 where  -- One element after the last one defined bu this chain fragment.
        ixHigh  = ix0 +# len

        -- NOTE: Don't put an "INLINE go" pragma here or GHC will 
        --       eta-expand the call to 'go' in a bad way involving
        --       casts.
        go ix !s
               | ix >=# ixHigh  
               = return ()

               | otherwise
               = case next ix s of
                        Yield !s' x
                         -> do  write ix x
                                go (ix +# 1#) s'

                        Update !s'
                         ->     go ix s'
{-# INLINE [1] evalM' #-}
