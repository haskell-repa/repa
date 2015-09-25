
module Data.Repa.Flow.Generic.Process
        ( -- * Processing
          compact_i

          -- * Scanning
        , scan_i

          -- * Indexed streams
        , indexed_i)
where
import Data.Repa.Flow.Generic.Base


-- | Combination of 'fold' and 'filter'.
--
--   We walk over the stream start-to-end, maintaining an accumulator.
--   At each point we can chose to emit an element, or not.
--
compact_i
        :: (Monad m, States i m)
        => (s -> a -> (s, Maybe b))
        -> s
        -> Sources i m a 
        -> m (Sources i m b)

compact_i f s0 (Sources n pullA)
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
{-# INLINE compact_i #-}


-- | Start-to-end scan over each stream in a bundle.
scan_i 
        :: (Monad m, States i m)
        => (s -> a -> s)
        -> s
        -> Sources i m a
        -> m (Sources i m s)

scan_i f s0 ss
 = compact_i work_scan s0 ss
 where
        work_scan s x 
         = let s' = f s x
           in  (s', Just s')
        {-# INLINE work_scan #-}
{-# INLINE scan_i #-}


-- | For each stream in a bundle of sources, 
--   associated the element with their corresponding position in the stream.
-- 
indexed_i
        :: (Monad m, States i m)
        => Sources i m a
        -> m (Sources i m (Int, a))

indexed_i ss
 = compact_i work_indexed 0 ss
 where
        work_indexed s x
         = (s + 1, Just (s, x))
        {-# INLINE work_indexed #-}
{-# INLINE indexed_i #-}

