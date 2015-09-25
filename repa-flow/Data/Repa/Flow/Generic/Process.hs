
module Data.Repa.Flow.Generic.Process
        ( -- * Processing
          process_i

          -- * Indexed streams
        , indexed_i)
where
import Data.Repa.Flow.Generic.Base


-- | Apply a co-recursive stream process to a bundle of sources.
--
--   The stepper function takes a state, and a value from the source,
--   and produces a new state and maybe a new value.
-- 
process_i
        :: (Monad m, States i m)
        => (s -> a -> (s, Maybe b))
        -> s
        -> Sources i m a 
        -> m (Sources i m b)

process_i f s0 (Sources n pullA)
 = do
        refs    <- newRefs n s0

        let pull_process i eatB ejectB
             = do s1    <- readRefs refs i
                  pullA i (eatA_process s1) ejectA_process

             where eatA_process s1 xA
                    = do case f s1 xA of
                          (s2', Nothing) 
                           -> do writeRefs refs i s2'
                                 pull_process i eatB ejectB

                          (s2', Just xB)
                           -> do writeRefs refs i s2'
                                 eatB xB
                   {-# INLINE eatA_process #-}

                   ejectA_process = ejectB
                   {-# INLINE ejectA_process #-}
            {-# INLINE pull_process #-}

        return $ Sources n pull_process
{-# INLINE process_i #-}


-- | For each stream in a bundle of sources, 
--   associated the element with their corresponding position in the stream.
-- 
indexed_i
        :: (Monad m, States i m)
        => Sources i m a
        -> m (Sources i m (Int, a))

indexed_i ss
 = process_i (\s x -> (s + 1, Just (s, x))) 0 ss
{-# INLINE indexed_i #-}

