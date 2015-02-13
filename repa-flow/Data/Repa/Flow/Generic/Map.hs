
module Data.Repa.Flow.Generic.Map
        ( map_i,        map_o
        , smap_i,       smap_o

        , szipWith_ii,  szipWith_io,    szipWith_oi)
where
import Data.Repa.Flow.Generic.Base
import Control.Monad
import Prelude                                  as P
#include "repa-flow.h"


-- | Apply a function to every element pulled from some sources, 
--   producing some new sources. 
map_i   :: Monad m
        => (a -> b) -> Sources i m a -> m (Sources i m b)
map_i f s = smap_i (\_ x -> f x) s
{-# INLINE map_i #-}


-- | Like `map_i`, but the worker function is also given the stream index.
smap_i  :: Monad m
        => (i -> a -> b) -> Sources i m a -> m (Sources i m b)
smap_i f (Sources n pullsA)
 = return $ Sources n pullsB_map
 where  
        pullsB_map i eat eject
         = pullsA  i eat_a eject_a
         where  
                eat_a v = eat (f i v)
                {-# INLINE eat_a #-}

                eject_a = eject
                {-# INLINE eject_a #-}

        {-# INLINE [1] pullsB_map #-}
{-# INLINE_FLOW smap_i #-}


-- | Apply a function to every element pulled from some sources, 
--   producing some new sources. 
map_o   :: Monad m
        => (a -> b) -> Sinks i m b -> m (Sinks i m a)
map_o f k = smap_o (\_ x -> f x) k
{-# INLINE map_o #-}


-- | Like `map_o`, but the worker function is also given the stream index.
smap_o   :: Monad m
        => (i -> a -> b) -> Sinks i m b -> m (Sinks i m a)
smap_o f (Sinks n pushB ejectB)
 = return $ Sinks n pushA_map ejectA_map
 where  
        pushA_map i a   = pushB  i (f i a)
        {-# INLINE pushA_map #-}

        ejectA_map i    = ejectB i
        {-# INLINE ejectA_map #-}
{-# INLINE_FLOW smap_o #-}


-- | Combine the elements of two flows with the given function.
--   The worker function is also given the stream index.
szipWith_ii 
        :: (Ord i, Monad m)
        => (i -> a -> b -> c)
        -> Sources i m a -> Sources i m b
        -> m (Sources i m c)

szipWith_ii f (Sources nA pullA) (Sources nB pullB)
 = return $ Sources (min nA nB) pull_szipWith
 where
        pull_szipWith i eat eject
         = pullA i eatA  eject
         where   
                eatA xA = pullB i eatB eject
                 where
                        eatB xB = eat (f i xA xB)
                        {-# INLINE eatB #-}
                {-# INLINE eatA #-}
        {-# INLINE pull_szipWith #-}
{-# INLINE_FLOW szipWith_ii #-}


-- | Like `szipWith_ii`, but take a bundle of `Sinks` for the result
--   elements, and yield a bundle of `Sinks` to accept the @b@ elements.
szipWith_io 
        :: (Ord i, Monad m)
        => (i -> a -> b -> c)
        -> Sinks i m c -> Sources i m a 
        -> m (Sinks i m b)

szipWith_io f (Sinks nC pushC ejectC) (Sources nA pullA)
 = return $ Sinks nB pushB ejectC
 where
        !nB = min nC nA

        pushB i xB 
         | i > nB       = return ()
         | otherwise    = pullA i eatA (ejectC i)
         where
                eatA xA = pushC i (f i xA xB)
                {-# INLINE eatA #-}
        {-# INLINE pushB #-}
{-# INLINE_FLOW szipWith_io #-}


-- | Like `szipWith_ii`, but take a bundle of `Sinks` for the result
--   elements, and yield a bundle of `Sinks` to accept the @a@ elements.
szipWith_oi
        :: (Ord i, Monad m)
        => (i -> a -> b -> c)
        -> Sinks i m c -> Sources i m b 
        -> m (Sinks i m a)

szipWith_oi f (Sinks nC pushC ejectC) (Sources nB pullB)
 = return $ Sinks nA pushA ejectC
 where
        !nA = min nC nB

        pushA i xA
         | i > nA       = return ()
         | otherwise    = pullB i eatB (ejectC i)
         where
                eatB xB = pushC i (f i xA xB)
                {-# INLINE eatB #-}
        {-# INLINE pushA #-}
{-# INLINE_FLOW szipWith_oi #-}

