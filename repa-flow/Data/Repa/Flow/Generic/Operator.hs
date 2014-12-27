
module Data.Repa.Flow.Generic.Operator
        ( -- * Constructors
          repeat_i
        , replicate_i
        , prepend_i

          -- * Mapping
        , map_i,        map_o

          -- * Connecting
        , dup_oo,       dup_io,         dup_oi
        , connect_i

          -- * Splitting
        , head_i

          -- * Watching
        , watch_i
        , watch_o
        , trigger_o

          -- * Discard and Ignore
        , discard_o
        , ignore_o)
where
import Data.Repa.Flow.Generic.List
import Data.Repa.Flow.Generic.Base


-- Constructors ---------------------------------------------------------------
-- | Yield sources that always produce the same value.
repeat_i :: Monad m
         => i -> (Ix i -> a) 
         -> m (Sources i m a)
repeat_i n f
 = return $ Sources n pull_repeat
 where  pull_repeat i eat _eject
          = eat (f i)
        {-# INLINE pull_repeat #-}
{-# INLINE [2] repeat_i #-}


-- | Yield sources of the given length that always produce the same value.
replicate_i 
        :: States i m
        => i -> Int -> (Ix i -> a) 
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
{-# INLINE [2] replicate_i #-}


-- | Prepend some more elements into the front of the sources.
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
{-# INLINE [2] prepend_i #-}


-- Mapping --------------------------------------------------------------------
-- | Apply a function to every element pulled from some sources, 
--   producing some new sources.
map_i   :: Monad m
        => (Ix i -> a -> b) -> Sources i m a -> m (Sources i m b)
map_i f (Sources n pullsA)
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
{-# INLINE [2] map_i #-}


-- | Apply a function to every element pushed to some sink,
--   producing a new sink.
map_o   :: Monad m
        => (Ix i -> a -> b) -> Sinks i m b -> m (Sinks i m a)
map_o f (Sinks n pushB ejectB)
 = return $ Sinks n pushA_map ejectA_map
 where  
        pushA_map i a   = pushB  i (f i a)
        {-# INLINE pushA_map #-}

        ejectA_map i    = ejectB i
        {-# INLINE ejectA_map #-}
{-# INLINE [2] map_o #-}



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
{-# INLINE [2] dup_oo #-}


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

        {-# INLINE [1] pull_dup #-}
{-# INLINE [2] dup_io #-}


-- | Send the same data to two consumers.
--
--   Like `dup_io` but with the arguments flipped.
--
dup_oi  :: (Ord i, Monad m)
        => Sinks i m a -> Sources i m a -> m (Sources i m a)
dup_oi sink1 source2 = dup_io source2 sink1
{-# INLINE [2] dup_oi #-}


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

{-# INLINE [2] connect_i #-}


-- Splitting ------------------------------------------------------------------
-- | Split the given number of elements from the head of a source 
--   returning those elements in a list, and producing a new source 
--   for the rest.
head_i  :: States i m
        => Int -> Sources i m a -> Ix i -> m ([a], Sources i m a)
head_i len s0 i
 = do   
        (s1, s2) <- connect_i s0
        xs       <- takeList1 len s1 i
        return   (xs, s2)
{-# INLINE [2] head_i #-}



-- Watch ----------------------------------------------------------------------
-- | Apply a monadic function to every element pulled from some sources,
--   producing some new sources.
watch_i  :: Monad m 
        => (Ix i -> a -> m ()) 
        -> Sources i m a  -> m (Sources i m a)

watch_i f (Sources n pullX) 
 = return $ Sources n pull_watch
 where  
        pull_watch i eat eject
         = pullX i eat_watch eject_watch
         where
                eat_watch x     = f i x >> eat x
                eject_watch     = eject
        {-# INLINE [1] pull_watch #-}
{-# INLINE [2] watch_i #-}


-- | Pass elements to the provided action as they are pushed into the sink.
watch_o :: Monad m 
        => (Ix i -> a -> m ())
        -> Sinks i m a ->  m (Sinks i m a)

watch_o f  (Sinks n push eject)
 = return $ Sinks n push_watch eject_watch
 where
        push_watch  !i !x = f i x >> push i x
        eject_watch !i    = eject i
{-# INLINE [2] watch_o #-}


-- | Like `watch` but doesn't pass elements to another sink.
trigger_o :: Monad m 
          => i -> (Ix i -> a -> m ()) -> m (Sinks i m a)
trigger_o i f
 = discard_o i >>= watch_o f
{-# INLINE [2] trigger_o #-}


-- Discard --------------------------------------------------------------------
-- | A sink that drops all data on the floor.
--
--   This sink is strict in the elements, so they are demanded before being
--   discarded. Haskell debugging thunks attached to the elements will be demanded.
discard_o :: Monad m 
          => i -> m (Sinks i m a)
discard_o n
 = return $ Sinks n push_discard eject_discard
 where  
        -- IMPORTANT: push_discard should be strict in the element so that
        -- and Haskell tracing thunks attached to it are evaluated.
        -- We *discard* the elements, but don't completely ignore them.
        push_discard  !_ !_ = return ()
        eject_discard !_    = return ()
{-# INLINE [2] discard_o #-}


-- | A sink that ignores all incoming elements.
--
--   This sink is non-strict in the elements. 
--   Haskell tracing thinks attached to the elements will *not* be demanded.
ignore_o  :: Monad m 
          => i -> m (Sinks i m a)
ignore_o n
 = return $ Sinks n push_ignore eject_ignore
 where
        push_ignore  _ _   = return ()
        eject_ignore _     = return ()
{-# INLINE [2] ignore_o #-}

