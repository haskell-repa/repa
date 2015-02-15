module Data.Repa.Flow.Generic.Connect
        ( -- * Dup
          dup_oo
        , dup_io
        , dup_oi

          -- * Connect
        , connect_i

          -- * Funnel
        , funnel_o)
where
import Data.Repa.Flow.Generic.Base
import Control.Monad
import Prelude                                  as P
#include "repa-flow.h"


-- Dup ------------------------------------------------------------------------
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
--   Pulling an element from the result source pulls from the argument
--   source, and pushes that element to the sink, as well as returning it
--   via the result source.
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


-- Connect --------------------------------------------------------------------
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


-- Funneling ------------------------------------------------------------------
-- | Given a bundle of sinks consisting of a single stream, produce a new
--   bundle of the given arity that sends all data to the former, ignoring
--   the stream index.
--
--   The argument stream is ejected only when all of the streams in the 
--   result bundle have been ejected.
-- 
--   * Using this function in conjunction with parallel operators like
--     `drainP` introduces non-determinism. Elements pushed to different
--     streams in the result bundle could enter the single stream in the
--     argument bundle in any order.
--
-- @
-- > import Data.Repa.Flow.Generic
-- > import Data.Repa.Array.Material
-- > import Data.Repa.Nice
--  
-- > let things = [(0 :: Int, \"foo\"), (1, \"bar\"), (2, \"baz\")]
-- > result \<- capture_o B () (\\k -> funnel_o 4 k >>= pushList things)
-- > nice result
-- [((),\"foo\"),((),\"bar\"),((),\"baz\")]
-- @
--
funnel_o :: States i m
         => i -> Sinks () m a -> m (Sinks i m a)
funnel_o nSinks (Sinks _ pushX ejectX)
 = do
        -- Refs to track which streams have been ejected.
        refs    <- newRefs nSinks False

        -- Push all received data into the single stream of the
        -- argument bundle.
        let push_funnel _ x 
             = pushX () x
            {-# INLINE push_funnel #-}

        -- When all the result streams have been ejected, 
        -- eject the argument stream.
        let eject_funnel i
             = do 
                  -- RACE: If two concurrent processes eject the final two
                  -- streams then they will both think they were the last
                  -- one, and eject the single argument stream. This is ok
                  -- as we allow the argument sink to be ejected multiple
                  -- times.
                  -- 
                  -- See docs of `Sinks` type in "Data.Repa.Flow.Generic.Base".
                  --
                  writeRefs refs i True
                  done  <- foldRefsM (&&) True refs
                  when done $ ejectX ()
            {-# INLINE eject_funnel #-}

        return $ Sinks nSinks push_funnel eject_funnel
{-# INLINE_FLOW funnel_o #-}

