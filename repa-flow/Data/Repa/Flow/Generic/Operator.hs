{-# OPTIONS -fno-warn-unused-imports #-}
module Data.Repa.Flow.Generic.Operator
        ( -- * Projection
          project_i
        , project_o

          -- * Funneling
        , funnel_o

          -- * Constructors
        , repeat_i
        , replicate_i
        , prepend_i,    prependOn_i

          -- * Splitting
        , head_i

          -- * Grouping
        , groups_i

          -- * Packing
        , pack_ii

          -- * Folding
        , folds_ii

          -- * Watching
        , watch_i
        , watch_o
        , trigger_o

          -- * Capturing
        , capture_o
        , rcapture_o

          -- * Ignorance
        , ignore_o
        , abandon_o

          -- * Tracing
        , trace_o)
where
import Data.Repa.Flow.Generic.Eval
import Data.Repa.Flow.Generic.List
import Data.Repa.Flow.Generic.Connect
import Data.Repa.Flow.Generic.Base
import Data.Repa.Array.Generic.Index            as A
import Data.Repa.Array.Generic                  as A
import Data.IORef
import Control.Monad
import Debug.Trace
import GHC.Exts
import Prelude                                  as P
#include "repa-flow.h"


-- Projection -----------------------------------------------------------------
-- | Project out a single stream source from a bundle.
project_i :: Monad m
          => i -> Sources i m a -> m (Sources () m a)
project_i ix (Sources _ pull)
 = return $ Sources () pull_project
 where  pull_project _ eat eject
         = pull ix eat eject
{-# INLINE_FLOW project_i #-}


-- | Project out a single stream sink from a bundle.
project_o :: Monad m
          => i -> Sinks i m a -> m (Sinks () m a)
project_o ix (Sinks _ push eject)
 = return $ Sinks () push_project eject_project
 where
        push_project _ v = push  ix v
        eject_project _  = eject ix
{-# INLINE_FLOW project_o #-}


-- Constructors ---------------------------------------------------------------
-- | Yield sources that always produce the same value.
repeat_i :: Monad m
         => i -> (i -> a) 
         -> m (Sources i m a)
repeat_i n f
 = return $ Sources n pull_repeat
 where  pull_repeat i eat _eject
          = eat (f i)
        {-# INLINE pull_repeat #-}
{-# INLINE_FLOW repeat_i #-}


-- | Yield sources of the given length that always produce the same value.
replicate_i 
        :: States i m
        => i -> Int -> (i -> a) 
        -> m (Sources i m a)

replicate_i n len f
 = do   
        refs   <- newRefs n 0
        let pull_replicate i eat eject
             = do !n' <- readRefs refs i
                  if n' >= len
                   then eject
                   else eat (f i)
            {-# INLINE pull_replicate #-}

        return $ Sources n pull_replicate
{-# INLINE_FLOW replicate_i #-}


-- | Prepend some more elements into the front of some sources.
prepend_i :: States i m
          => [a] -> Sources i m a -> m (Sources i m a)
prepend_i xs (Sources n pullX)
 = do   
        refs    <- newRefs n xs

        let pull_prepend i eat eject
             = do xs'   <- readRefs refs i
                  case xs' of
                   x : xs'' -> do 
                         writeRefs refs i xs''
                         eat x

                   [] -> pullX i eat eject
            {-# INLINE pull_prepend #-}

        return (Sources n pull_prepend)
{-# INLINE_FLOW prepend_i #-}


-- | Like `prepend_i` but only prepend the elements to the streams
--   that match the given predicate.
prependOn_i 
        :: States i m
        => (i -> Bool) -> [a] -> Sources i m a -> m (Sources i m a)
prependOn_i p xs (Sources n pullX)
 = do   
        refs    <- newRefs n xs

        let pull_prependOn i eat eject
             | p i
             = do xs'   <- readRefs refs i
                  case xs' of
                   x : xs'' -> do 
                         writeRefs refs i xs''
                         eat x

                   [] -> pullX i eat eject

             | otherwise
             = pullX i eat eject
            {-# INLINE pull_prependOn #-}

        return (Sources n pull_prependOn)
{-# INLINE_FLOW prependOn_i #-}


-- Splitting ------------------------------------------------------------------
-- | Split the given number of elements from the head of a source 
--   returning those elements in a list, and yielding a new source 
--   for the rest.
head_i  :: States i m
        => Int -> Sources i m a -> i -> m ([a], Sources i m a)
head_i len s0 i
 = do   
        (s1, s2) <- connect_i s0
        xs       <- takeList1 len i s1
        return   (xs, s2)
{-# INLINE head_i #-}


-- Groups ---------------------------------------------------------------------
-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
-- 
--   Example: groups [4, 4, 4, 3, 3, 1, 1, 1, 4] = [3, 2, 3, 1]
--
groups_i 
        :: (Ord i, Monad m, Eq a)
        => Sources i m a -> m (Sources i m Int)

groups_i (Sources n pullV)
 = return $ Sources n pull_n
 where  
        -- Pull a whole run from the source, so that we can produce.
        -- the output element. 
        pull_n i eat eject
         = loop_groups Nothing 1#
         where 
                loop_groups !mx !count
                 = pullV i eat_v eject_v
                 where  eat_v v
                         = case mx of
                            -- This is the first element that we've read from
                            -- the source.
                            Nothing -> loop_groups (Just v) count

                            -- See if the next element is the same as the one
                            -- we read previously
                            Just x  -> if x == v
                                        then loop_groups (Just x) (count +# 1#)
                                        else eat (I# count)  
                                        -- TODO: ** STORE PULLED VALUE FOR LATER
                        {-# INLINE eat_v #-}

                        eject_v 
                         = case mx of
                            -- We've already written our last count, 
                            -- and there are no more elements in the source.
                            Nothing -> eject

                            -- There are no more elements in the source,
                            -- so emit the final count
                            Just _  -> eat (I# count)
                        {-# INLINE eject_v #-}
                {-# INLINE loop_groups #-}
        {-# INLINE pull_n #-}
{-# INLINE_FLOW groups_i #-}


-- Pack -----------------------------------------------------------------------
-- | Given a stream of flags and a stream of values, produce a new stream
--   of values where the corresponding flag was True. The length of the result
--   is the length of the shorter of the two inputs.
pack_ii :: (Ord i, Monad m)
        => Sources i m Bool -> Sources i m a -> m (Sources i m a)

pack_ii (Sources nF pullF) (Sources nX pullX)
 = return $ Sources (min nF nX) pull_pack
 where   
        pull_pack i eat eject
         = pullF i eat_f eject_f
         where eat_f f        = pack_x f
               eject_f        = eject

               pack_x f
                = pullX i eat_x eject_x
                where eat_x x = if f then eat x
                                     else pull_pack i eat eject

                      eject_x = eject
               {-# INLINE pack_x #-}
        {-# INLINE pull_pack #-}
{-# INLINE_FLOW pack_ii #-}


-- Folds ----------------------------------------------------------------------
-- | Segmented fold. 
folds_ii 
        :: (Ord i, Monad m)
        => (a -> a -> a) -> a
        -> Sources i m Int 
        -> Sources i m a 
        -> m (Sources i m a)

folds_ii f z (Sources nL pullLen)
             (Sources nX pullX)
 = return $   Sources (min nL nX) pull_folds
 where  
        pull_folds i eat eject
         = pullLen i eat_len eject_len
         where 
               eat_len (I# len) = loop_folds len z
               eject_len        = eject
                   
               loop_folds !c !acc
                | tagToEnum# (c ==# 0#) = eat acc
                | otherwise
                = pullX i eat_x eject_x
                where 
                      eat_x x = loop_folds (c -# 1#) (f acc x)
                      eject_x = eject
               {-# INLINE loop_folds #-} 
        {-# INLINE pull_folds #-}
{-# INLINE_FLOW folds_ii #-}


-- Watch ----------------------------------------------------------------------
-- | Apply a monadic function to every element pulled from some sources,
--   producing some new sources.
watch_i :: Monad m 
        => (i -> a -> m ()) 
        -> Sources i m a  -> m (Sources i m a)

watch_i f (Sources n pullX) 
 = return $ Sources n pull_watch
 where  
        pull_watch i eat eject
         = pullX i eat_watch eject_watch
         where
                eat_watch x     = f i x >> eat x
                eject_watch     = eject
        {-# INLINE pull_watch #-}
{-# INLINE_FLOW watch_i #-}


-- | Pass elements to the provided action as they are pushed into the sink.
watch_o :: Monad m 
        => (i -> a -> m ())
        -> Sinks i m a ->  m (Sinks i m a)

watch_o f  (Sinks n push eject)
 = return $ Sinks n push_watch eject_watch
 where
        push_watch  !i !x = f i x >> push i x
        eject_watch !i    = eject i
{-# INLINE_FLOW watch_o #-}


-- | Create a bundle of sinks of the given arity and capture any 
--   data pushed to it.
--
-- @ 
-- > import Data.Repa.Flow.Generic
-- > import Data.Repa.Array.Material
-- > import Data.Repa.Nice
-- > import Control.Monad
-- > liftM nice $ capture_o B 4 (\k -> pushList [(0 :: Int, "foo"), (1, "bar"), (0, "baz")] k)
-- > [(0,"foo"),(1,"bar"),(0,"baz")]
-- @
--
---
--   TODO: avoid going via lists when accumulating.
--
capture_o 
        :: (Target lDst (i, a), Index lDst ~ Int)
        => Name lDst               -- ^ Name of desination layout.
        -> i                       -- ^ Arity of result bundle.
        -> (Sinks i IO a -> IO ()) -- ^ Function to push data into the sinks.
        -> IO (Array lDst (i, a))

capture_o nDst n use
 = liftM fst $ rcapture_o nDst n use
{-# INLINE capture_o #-}


-- | Like `capture_o` but also return the @r@-esult of the push function.
rcapture_o 
        :: (Target lDst (i, a), Index lDst ~ Int)
        => Name lDst               -- ^ Name of desination layout.
        -> i                       -- ^ Arity of result bundle.
        -> (Sinks i IO a -> IO b)  -- ^ Function to push data into the sinks.
        -> IO (Array lDst (i, a), b)

rcapture_o nDst n use
 = do   
        ref      <- newIORef []

        let capture_eat i x
             = atomicModifyIORef ref (\old -> ((i, x) : old, ()))
            {-# INLINE capture_eat #-}

        k0       <- ignore_o n
        k1       <- watch_o   capture_eat k0

        x        <- use k1
        result   <- readIORef ref
        let !arr =  A.fromList nDst $ P.reverse result

        return (arr, x)
{-# INLINE_FLOW rcapture_o #-}


-- | Like `watch_o` but doesn't pass elements to another sink.
trigger_o :: Monad m 
          => i -> (i -> a -> m ()) -> m (Sinks i m a)
trigger_o i f
 = ignore_o i >>= watch_o f
{-# INLINE trigger_o #-}


-- Ignorance-------------------------------------------------------------------
-- | A sink that ignores all incoming data.
--
--   * This sink is strict in the elements, so they are demanded before being
--     discarded. 
--   * Haskell debugging thunks attached to the elements will be
--     demanded.
--
ignore_o  :: Monad m 
          => i -> m (Sinks i m a)
ignore_o n
 = return $ Sinks n push_ignore eject_ignore
 where  
        -- IMPORTANT: push_ignore should be strict in the element so that
        -- and Haskell tracing thunks attached to it are evaluated.
        push_ignore  !_ !_ = return ()
        eject_ignore !_    = return ()
{-# INLINE_FLOW ignore_o #-}


-- | A sink that drops all data on the floor.
--
--   * This sink is non-strict in the elements. 
--   * Haskell tracing thunks attached to the elements will *not* be demanded.
--
abandon_o :: Monad m 
          => i -> m (Sinks i m a)
abandon_o n
 = return $ Sinks n push_abandon eject_abandon
 where
        push_abandon  _ _   = return ()
        eject_abandon _     = return ()
{-# INLINE_FLOW abandon_o #-}


-- Trace ----------------------------------------------------------------------
-- | Use the `trace` function from "Debug.Trace" to print each element
--   that is pushed to a sink.
--
--   * This function is intended for debugging only, and is not intended
--     for production code.
--
trace_o :: (Show i, Show a, Monad m)
        => i -> m (Sinks i m a)

trace_o nSinks 
 = trigger_o nSinks eat
 where
        eat i x
         = trace ("repa-flow trace_o: " ++ show i ++ "; " ++ show x)
                 (return ())

{-# NOINLINE trace_o #-}

