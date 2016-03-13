
module Data.Repa.Chain.Scan
        ( unfoldsC
        , scanMaybeC
        , groupsByC)
where
import Data.Repa.Chain.Base
import qualified Data.Vector.Fusion.Stream.Size  as S
#include "repa-stream.h"


-------------------------------------------------------------------------------
-- | Segmented unfold.
-- 
--   The worker function takes the current element from the input stream 
--   and current state. 
--
--   If the worker returns Just x then that output element will be placed
--   in the output stream, and it will be called again with the same
--   input elemenent and next state. 
--
--   If the worker returns Nothing then we advance to the next element
--   of the input stream.
--
unfoldsC
        :: Monad m
        => (a -> k -> m (k, Maybe b))   -- ^ Worker function.
        -> k                            -- ^ Initial state for the unfold.
        -> Chain m s               a    -- ^ Input elements.
        -> Chain m (s, k, Maybe a) b    -- ^ Output elements.

unfoldsC f k0 (Chain _ s0 istep)
 = Chain S.Unknown (s0, k0, Nothing) ostep
 where
        ostep  (s1, k1, Nothing)
         =  istep s1 >>= \rs
         -> case rs of
                Yield xa s2     -> return $ Skip  (s2, k1, Just xa)
                Skip     s2     -> return $ Skip  (s2, k1, Nothing)
                Done     s2     -> return $ Done  (s2, k1, Nothing)

        ostep  (s1, k1, Just xa)
         = f xa k1   >>= \kmb
         -> case kmb of
                (k2, Nothing)   -> return $ Skip     (s1, k2, Nothing)
                (k2, Just xb)   -> return $ Yield xb (s1, k2, Just xa)
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM unfoldsC #-}


-------------------------------------------------------------------------------
-- | Perform a left-to-right scan through an input vector, maintaining a state
--   value between each element. For each element of input we may or may not
--   produce an element of output.
scanMaybeC 
        :: Monad m
        => (k -> a -> m (k, Maybe b))   -- ^ Worker function.
        ->  k                           -- ^ Initial state for scan.
        -> Chain m s      a             -- ^ Input elements.
        -> Chain m (s, k) b             -- ^ Output elements and final state.

scanMaybeC f k0 (Chain sz s0 istep)
 = Chain (S.toMax sz) (s0, k0) ostep
 where
        ostep  (s1, k1)
         =  istep s1 >>= \rs
         -> case rs of
                Yield x s2      
                 -> f k1 x >>= \rk
                 -> case rk of
                        (k2, Nothing) -> return $ Skip    (s2, k2)
                        (k2, Just y)  -> return $ Yield y (s2, k2)

                Skip s2               -> return $ Skip    (s2, k1)
                Done s2               -> return $ Done    (s2, k1)
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM scanMaybeC #-}


-------------------------------------------------------------------------------
-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
groupsByC
        :: Monad m
        => (a -> a -> m Bool)           -- ^ Comparison function.
        -> Maybe (a, Int)               -- ^ Starting element and count.
        -> Chain m  s a                 -- ^ Input elements.
        -> Chain m (s, Maybe (a, Int)) (a, Int) 
                 
groupsByC f !s !vec
 = scanMaybeC work_groupsByC s vec
 where  
        work_groupsByC !acc !y
         = case acc of
                Nothing      
                 -> return $ (Just (y, 1),     Nothing)

                Just (x, n)
                 -> f x y >>= \rk
                 -> if rk 
                        then return (Just (x, n + 1), Nothing)
                        else return (Just (y, 1),     Just (x, n))
        {-# INLINE_INNER work_groupsByC #-}
{-# INLINE_STREAM groupsByC #-}

