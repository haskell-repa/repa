
module Data.Repa.Flow.Generic.Eval
        ( drainS
        , drainP
        , consumeS)
where
import Data.Repa.Flow.Generic.Base
import Data.Repa.Eval.Gang                      as Eval
import GHC.Exts
#include "repa-flow.h"


-- | Pull all available values from the sources and push them to the sinks.
--   Streams in the bundle are processed sequentially, from first to last.
--
--   * If the `Sources` and `Sinks` have different numbers of streams then
--     we only evaluate the common subset.
--
drainS  :: (Next i, Monad m)
        => Sources i m a -> Sinks i m a -> m ()

drainS (Sources nSources ipull) (Sinks nSinks opush oeject)
 = loop_drain first
 where 
        n = min nSources nSinks

        loop_drain !ix
         = ipull ix eat_drain eject_drain
         where  eat_drain v
                 = do   opush ix v
                        loop_drain ix
                {-# INLINE eat_drain #-}

                eject_drain
                 = do   oeject ix  
                        case next ix n of
                         Nothing        -> return ()
                         Just ix'       -> loop_drain ix'
                {-# INLINE eject_drain #-}
        {-# INLINE loop_drain #-}
{-# INLINE_FLOW drainS #-}


-- | Pull all available values from the sources and push them to the sinks,
--   in parallel. We fork a thread for each of the streams and evaluate
--   them all in parallel.
--
--   * If the `Sources` and `Sinks` have different numbers of streams then
--     we only evaluate the common subset.
--
drainP  :: Sources Int IO a -> Sinks Int IO a -> IO ()
drainP (Sources nSources ipull) (Sinks nSinks opush oeject)
 = do   
        -- Create a new gang.
        gang    <- Eval.forkGang n

        -- Evalaute all the streams in different threads.
        Eval.gangIO gang drainMe

 where  
        !n      = min nSources nSinks

        drainMe !ix
         = ipull (I# ix) eat_drain eject_drain
         where  eat_drain v 
                 = do   opush  (I# ix) v
                        drainMe ix
                {-# INLINE eat_drain #-}

                eject_drain = oeject (I# ix)
                {-# INLINE eject_drain #-}
        {-# INLINE drainMe #-}
{-# INLINE_FLOW drainP #-}


-- | Pull all available values from the sources and pass them to the
--   given action.
consumeS :: (Next i, Monad m)
         => (i -> a -> m ())
         -> Sources i m a 
         -> m ()
consumeS eat (Sources nSources ipull)
 = loop_consume first
 where  
        loop_consume !ix
         = ipull ix eat_consume eject_consume
         where  
                eat_consume v
                 = do eat ix v
                      loop_consume ix
                {-# INLINE eat_consume #-}

                eject_consume 
                 = do case next ix nSources of
                        Nothing  -> return ()
                        Just ix' -> loop_consume ix'
                {-# INLINE eject_consume #-}
        {-# INLINE loop_consume #-}
{-# INLINE_FLOW consumeS #-}

