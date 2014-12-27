
module Data.Repa.Flow.Simple.Operator
        ( -- * Constructors
          repeat_i
        , replicate_i

          -- * Mapping
        , map_i,        map_o

          -- * Connecting
        , dup_oo,       dup_io,         dup_oi
        , connect_i)

{-
        , head_i
        , peek_i
        , pre_i
        , groups_i
        , pack_ii
        , folds_ii
        , watch_i
        , watch_o
        , trigger_o
        , discard_o
        , ignore_o
-}
where
import Data.Repa.Flow.Simple.Base
import Control.Monad
import Data.Repa.Flow.States                    (States (..))
import qualified Data.Repa.Flow.Generic         as G

{-
import Data.Repa.Flow.Simple.List
import Data.Repa.Flow.Simple.Base
import Control.Monad
import Prelude          hiding (length)
import GHC.Exts         hiding (toList)
import qualified Prelude        as P
-}

-- Constructors ---------------------------------------------------------------
-- | Yield a source that always produces the same value.
repeat_i :: States () m
         => a -> m (Source m a)
repeat_i x
        = liftM wrap $ G.repeat_i () (const x)
{-# INLINE [2] repeat_i #-}


-- | Yield a source of the given length that always produces the same value.
replicate_i 
        :: States () m
        => Int -> a -> m (Source m a)
replicate_i n x 
        = liftM wrap $ G.replicate_i () n (const x)
{-# INLINE [2] replicate_i #-}


-- Mapping --------------------------------------------------------------------
-- | Apply a function to every element pulled from some source, 
--   producing a new source.
map_i     :: States () m => (a -> b) -> Source m a -> m (Source m b)
map_i f s = liftM wrap $ G.map_i (\G.UIx x -> f x) $ unwrap s


-- | Apply a function to every element pushed to some sink,
--   producing a new sink.
map_o     :: States () m => (a -> b) -> Sink   m b -> m (Sink   m a)
map_o f s = liftM wrap $ G.map_o (\G.UIx x -> f x) $ unwrap s


-- Connecting -----------------------------------------------------------------
-- | Send the same data to two consumers.
--
--   Given two argument sinks, yield a result sink.
--   Pushing to the result sink causes the same element to be pushed to both
--   argument sinks. 
dup_oo    :: States () m => Sink m a   -> Sink m a -> m (Sink m a)
dup_oo o1 o2 = liftM wrap $ G.dup_oo (unwrap o1) (unwrap o2)


-- | Send the same data to two consumers.
--  
--   Given an argument source and argument sink, yield a result source.
--   Pulling an element from the result source pulls from the argument source,
--   and pushes that element to the sink, as well as returning it via the
--   result source.
dup_io    :: States () m => Source m a -> Sink m a -> m (Source m a)
dup_io i1 o2 = liftM wrap $ G.dup_io (unwrap i1) (unwrap o2)


-- | Send the same data to two consumers.
--
--   Like `dup_io` but with the arguments flipped.
--
dup_oi    :: States () m => Sink m a   -> Source m a -> m (Source m a)
dup_oi o1 i2 = liftM wrap $ G.dup_oi (unwrap o1) (unwrap i2)


-- | Connect an argument source to two result sources.
--
--   Pulling from either result source pulls from the argument source.
--   Each result source only gets the elements pulled at the time, 
--   so if one side pulls all the elements the other side won't get any.
connect_i :: States () m
          => Source m a -> m (Source m a, Source m a)
connect_i i1 = liftM wrap2 $ G.connect_i (unwrap i1)


{-
-- | Peek at the given number of elements in the stream, 
--   returning a result stream that still produces them all.
peek_i :: Int -> Source IO a -> IO ([a], Source IO a)
peek_i n s0
 = do
        (s1, s2) <- connect_i s0
        xs       <- takeList n s1
        s3       <- pre_i xs s2
        return   (xs, s3)
{-# NOINLINE peek_i #-}




-- Groups ---------------------------------------------------------------------
-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
-- 
--   Example: groups [4, 4, 4, 3, 3, 1, 1, 1, 4] = [3, 2, 3, 1]
--
groups_i 
        :: (Show a, Eq a) 
        => Source IO a -> IO (Source IO Int)

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
pack_ii :: Source s Bool -> Source s a -> IO (Source s a)
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
        :: Monad m
        => (a -> a -> a) 
        -> a
        -> Source m Int 
        -> Source m a 
        -> m (Source m a)

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

-}
