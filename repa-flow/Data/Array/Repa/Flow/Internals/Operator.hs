
module Data.Array.Repa.Flow.Internals.Operator
        ( repeat_i
        , replicate_i
        , map_i,        map_o
        , dup_oo,       dup_io,         dup_oi
        , connect_i
        , head_i
        , peek_i
        , pre_i
        , groups_i
        , pack_ii
        , folds_ii
        , trigger_o)
where
import Data.Array.Repa.Flow.Internals.List
import Data.Array.Repa.Flow.Internals.Base
import Control.Monad
import Data.IORef
import Prelude          hiding (length)
import GHC.Exts         hiding (toList)
import qualified Prelude        as P


-- Repeat ---------------------------------------------------------------------
-- | Produce an flow that always produces the same value.
repeat_i :: a -> IO (Source a)
repeat_i x
 = return $ Source pull_repeat
 where  pull_repeat eat _eject
          = eat x
        {-# INLINE pull_repeat #-}
{-# INLINE [2] repeat_i #-}


-- Replicate ------------------------------------------------------------------
-- | Produce a flow of the given length that always produces the same value.
replicate_i :: Int -> a -> IO (Source a)
replicate_i len x
 = do   
        ref     <- newIORef 0

        let pull_replicate eat eject
             = do !ix   <- readIORef ref
                  if ix >= len
                   then eject
                   else eat x
            {-# INLINE pull_replicate #-}

        return $ Source pull_replicate

{-# INLINE [2] replicate_i #-}


-- Map ------------------------------------------------------------------------
-- | Apply a function to every element pulled from some stream source.
--   producing a new stream source.
map_i   :: (a -> b) -> Source a -> IO (Source b)
map_i f (Source pullA) 
 = return $ Source pullB_map
 where 
        pullB_map eat eject
         = pullA eat_a eject_a
         where 
              eat_a v = eat (f v)
              {-# INLINE eat_a #-}
              
              eject_a = eject
              {-# INLINE eject_a #-}

        {-# INLINE [1] pullB_map #-}

{-# INLINE [2] map_i #-}


-- | Apply a function to every element pushed to some stream sink,
--   producing a new stream sink.
map_o   :: (a -> b) -> Sink b -> IO (Sink a)
map_o f (Sink pushB ejectB)
 = return $ Sink pushA_map ejectA_map
 where  
        pushA_map a   = pushB (f a)
        {-# INLINE pushA_map #-}

        ejectA_map    = ejectB
        {-# INLINE ejectA_map #-}

{-# INLINE [2] map_o #-}


-- Dup ------------------------------------------------------------------------
-- | Send the same data to two consumers.
--
--   Given two argument sinks, produce a result sink.
--   Pushing to the result sink causes the same element to be pushed to both
--   argument sinks. 
dup_oo :: Sink a -> Sink a -> IO (Sink a)
dup_oo (Sink push1 eject1) (Sink push2 eject2)
 = return $ Sink push_dup eject_dup
 where  
        push_dup x  = push1 x >> push2 x
        {-# INLINE push_dup #-}

        eject_dup   = eject1  >> eject2
        {-# INLINE eject_dup #-}

{-# INLINE [2] dup_oo #-}


-- | Send the same data to two consumers.
--  
--   Given an argument source and argument sink, produce a result source.
--   Pulling an element from the result source pulls from the argument source,
--   and pushes that element to the sink, as well as returning it via the
--   result source.
--   
dup_io :: Source a -> Sink a -> IO (Source a)
dup_io (Source pull1) (Sink push2 eject2)
 = return $ Source pull_dup
 where
        pull_dup eat eject
         = pull1 eat_x eject_x
           where 
                 eat_x x = eat x >> push2 x
                 {-# INLINE eat_x #-}

                 eject_x = eject >> eject2
                 {-# INLINE eject_x #-}

        {-# INLINE [1] pull_dup #-}

{-# INLINE [2] dup_io #-}


-- | Send the same data to two consumers.
dup_oi :: Sink a -> Source a -> IO (Source a)
dup_oi sink1 source2 = dup_io source2 sink1
{-# INLINE [2] dup_oi #-}


-- Connect ---------------------------------------------------------------------
-- | Connect an argument source to two result sources.
--
--   Pulling from either result source pulls from the argument source.
--   Each result source only gets the elements pulled at the time, 
--   so if one side pulls all the elements the other side won't get any.
--
connect_i :: Source a -> IO (Source a, Source a)
connect_i (Source pullX)
 = do   
        refX    <- newIORef Nothing

        -- IMPORTANT: the pump function is set to NOINLINE so that pullX 
        -- will not be inlined into both consumers. We do not want to 
        -- duplicate that code for both result sources. Instead, calling
        -- pump writes its element into a ref, and then only the code
        -- that reads the ref is duplicated.
        let pump_connect
             = do pullX pump_eat pump_eject
            {-# NOINLINE pump_connect #-}

            pump_eat !x
             = writeIORef refX (Just x)
            {-# INLINE pump_eat #-}

            pump_eject
             = writeIORef refX Nothing
            {-# INLINE pump_eject #-}

        let pull_splitAt eat eject
             = do pump_connect
                  mx <- readIORef refX
                  case mx of
                   Just x    -> eat x
                   Nothing   -> eject
            {-# INLINE pull_splitAt #-}

        return ( Source pull_splitAt
               , Source pull_splitAt )

{-# NOINLINE connect_i #-}


-- Head -----------------------------------------------------------------------
-- | Split the given number of elements from the head of a source 
--   returning those elements in a list, and producing a new source 
--   for the rest.
head_i :: Int -> Source a -> IO ([a], Source a)
head_i n s0
 = do   
        (s1, s2) <- connect_i s0
        xs       <- takeList n s1
        return   (xs, s2)

{-# INLINE [2] head_i #-}


-- Peek -----------------------------------------------------------------------
-- | Peek at the given number of elements in the stream, 
--   returning a result stream that still produces them all.
peek_i :: Int -> Source a -> IO ([a], Source a)
peek_i n s0
 = do
        (s1, s2) <- connect_i s0
        xs       <- takeList n s1
        s3       <- pre_i xs s2
        return   (xs, s3)
{-# NOINLINE peek_i #-}



-- Prepend ----------------------------------------------------------------------
-- | Prepent some more elements into the front of an argument source,
--   producing a result source.
--
--   The results source returns the new elements, then the ones from
--   the argument source.
pre_i :: [a] -> Source a -> IO (Source a)
pre_i xs (Source pullX)
 = do   
        let !len  = P.length xs
        ref       <- newIORef 0

        let pull_stuff eat eject
             = do ix <- readIORef ref
                  if ix < len

                   then do writeIORef ref (ix + 1)
                           eat (xs !! ix)

                   else do writeIORef ref (ix + 1)
                           pullX eat eject

        return (Source pull_stuff)

{-# INLINE [2] pre_i #-}



-- Groups ---------------------------------------------------------------------
-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
-- 
--   Example: groups [4, 4, 4, 3, 3, 1, 1, 1, 4] = [3, 2, 3, 1]
--
groups_i 
        :: (Show a, Eq a) 
        => Source a -> IO (Source Int)

groups_i (Source pullV)
 = return $ Source pull_n
 where  
        -- Pull a whole run from the source, so that we can produce.
        -- the output element. 
        pull_n eat eject
         = loop_groups Nothing 1#
         where 
                loop_groups !mx !count
                 = pullV eat_v eject_v
                 where  eat_v v
                         = case mx of
                            -- This is the first element that we've read from
                            -- the source.
                            Nothing -> loop_groups (Just v) count

                            -- See if the next element is the same as the one
                            -- we read previously
                            Just x  -> if x == v
                                        then loop_groups (Just x) (count +# 1#)
                                        else eat (I# count)  -- TODO: ** STORE PULLED VALUE FOR LATER
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

        {-# INLINE [1] pull_n #-}

{-# INLINE [2] groups_i #-}


-- Pack -----------------------------------------------------------------------
-- | Given a stream of flags and a stream of values, produce a new stream
--   of values where the corresponding flag was True. The length of the result
--   is the length of the shorter of the two inputs.
pack_ii :: Source Bool -> Source a -> IO (Source a)
pack_ii (Source pullF) (Source pullX)
 = return $ Source pull_pack
 where   
        pull_pack eat eject
         = pullF eat_f eject_f
         where eat_f f        = pack_x f
               eject_f        = eject

               pack_x f
                = pullX eat_x eject_x
                where eat_x x = if f then eat x
                                     else pull_pack eat eject

                      eject_x = eject
               {-# INLINE [1] pack_x #-}

        {-# INLINE [1] pull_pack #-}

{-# INLINE [2] pack_ii #-}


-- Folds ----------------------------------------------------------------------
-- | Segmented fold. 
folds_ii 
        :: (a -> a -> a) 
        -> a
        -> Source Int 
        -> Source a 
        -> IO (Source a)

folds_ii f z (Source pullLen) (Source pullX)
 = return $ Source pull_folds
 where  
        pull_folds eat eject
         = pullLen eat_len eject_len
         where 
               eat_len (I# len) = loop_folds len z
               eject_len        = eject
                   
               loop_folds !n !acc
                | tagToEnum# (n ==# 0#) = eat acc
                | otherwise
                = pullX eat_x eject_x
                where 
                      eat_x x = loop_folds (n -# 1#) (f acc x)
                      eject_x = eject

        {-# INLINE [1] pull_folds #-}
{-# INLINE [2] folds_ii #-}


-- Trigger --------------------------------------------------------------------
-- | A sink that runs some IO action each time a value is pushed to it.
trigger_o :: (a -> IO ()) -> IO (Sink a)
trigger_o eat
 = do   let push_trigger x      = eat x
        let eject_trigger       = return ()
        return $ Sink push_trigger eject_trigger
{-# INLINE [2] trigger_o #-}
