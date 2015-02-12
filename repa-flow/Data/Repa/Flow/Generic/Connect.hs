module Data.Repa.Flow.Generic.Connect
        ( dup_oo
        , dup_io
        , dup_oi
        , connect_i)
where
import Data.Repa.Flow.Generic.Base
import Control.Monad
import Prelude                                  as P
#include "repa-flow.h"



-- Connect --------------------------------------------------------------------
-- | Send the same data to two consumers.
--
--   Given two argument sinks, yield a result sink.
--   Pushing to the result sink causes the same element to be pushed to both
--   argument sinks. 
dup_oo  :: (Ord i, States i m)
        => Sinks i m a -> Sinks i m a -> m (Sinks i m a)
dup_oo (Sinks n1 push1 eject1) (Sinks n2 push2 eject2)
 = return $ Sinks (min n1 n2) push_dup eject_dup
 where  
        push_dup i x  = push1 i x >> push2 i x
        {-# INLINE push_dup #-}

        eject_dup i   = eject1 i  >> eject2 i
        {-# INLINE eject_dup #-}
{-# INLINE_FLOW dup_oo #-}


-- | Send the same data to two consumers.
--  
--   Given an argument source and argument sink, yield a result source.
--   Pulling an element from the result source pulls from the argument source,
--   and pushes that element to the sink, as well as returning it via the
--   result source.
--   
dup_io  :: (Ord i, Monad m)
        => Sources i m a -> Sinks i m a -> m (Sources i m a)
dup_io (Sources n1 pull1) (Sinks n2 push2 eject2)
 = return $ Sources (min n1 n2) pull_dup
 where
        pull_dup i eat eject
         = pull1 i eat_x eject_x
           where 
                 eat_x x = eat x >> push2 i x
                 {-# INLINE eat_x #-}

                 eject_x = eject >> eject2 i
                 {-# INLINE eject_x #-}
        {-# INLINE pull_dup #-}
{-# INLINE_FLOW dup_io #-}


-- | Send the same data to two consumers.
--
--   Like `dup_io` but with the arguments flipped.
--
dup_oi  :: (Ord i, Monad m)
        => Sinks i m a -> Sources i m a -> m (Sources i m a)
dup_oi sink1 source2 = dup_io source2 sink1
{-# INLINE_FLOW dup_oi #-}


-- | Connect an argument source to two result sources.
--
--   Pulling from either result source pulls from the argument source.
--   Each result source only gets the elements pulled at the time, 
--   so if one side pulls all the elements the other side won't get any.
--
connect_i 
        :: States  i m
        => Sources i m a -> m (Sources i m a, Sources i m a)

connect_i (Sources n pullX)
 = do   
        refs    <- newRefs n Nothing

        -- IMPORTANT: the pump function is set to NOINLINE so that pullX 
        -- will not be inlined into both consumers. We do not want to 
        -- duplicate that code for both result sources. Instead, calling
        -- pump writes its element into a ref, and then only the code
        -- that reads the ref is duplicated.
        let pump_connect i
             = pullX i pump_eat pump_eject
             where
                pump_eat !x = writeRefs refs i (Just x)
                {-# INLINE pump_eat #-}

                pump_eject
                 = writeRefs refs i Nothing
                {-# INLINE pump_eject #-}
            {-# NOINLINE pump_connect #-}

        let pull_splitAt i eat eject
             = do pump_connect i
                  mx <- readRefs refs i
                  case mx of
                   Just x    -> eat x
                   Nothing   -> eject
            {-# INLINE pull_splitAt #-}

        return ( Sources n pull_splitAt
               , Sources n pull_splitAt )

{-# INLINE_FLOW connect_i #-}


