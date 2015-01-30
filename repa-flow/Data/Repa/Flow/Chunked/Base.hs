
module Data.Repa.Flow.Chunked.Base
        ( Sources, Sinks
        , Flow
        , fromList_i
        , fromLists_i
        , toList1_i
        , toLists1_i
        , head_i
        , finalize_i
        , finalize_o)
where
import qualified Data.Sequence                  as Q
import qualified Data.Foldable                  as Q
import Data.Repa.Flow.States
import Data.Repa.Array                          as A
import Data.Repa.Eval.Array                     as A
import qualified Data.Repa.Flow.Generic         as G
import Control.Monad
import Prelude                                  as P
#include "repa-stream.h"


-- | A bundle of sources, where the elements are chunked into arrays.
type Sources i m l e
        = G.Sources i m (A.Array l e)


-- | A bundle of sinks,   where the elements are chunked into arrays.
type Sinks   i m l e
        = G.Sinks   i m (A.Array l e)


-- | Shorthand for common type classes.
type Flow i m l a
        = (Ord i, Monad m, BulkI l a, States i m)


-- Conversion -----------------------------------------------------------------
-- | Given an arity and a list of elements, yield sources that each produce all
--   the elements. 
--
--   * All elements are stuffed into a single chunk, and each stream is given
--     the same chunk.
fromList_i 
        :: (States i m, A.TargetI l a)
        => Name l -> i -> [a] -> m (Sources i m l a)

fromList_i nDst n xs
 = G.fromList n [A.fromList nDst xs]
{-# INLINE fromList_i #-}


-- | Like `fromLists_i` but take a list of lists, where each of the inner
--   lists is packed into a single chunk.
fromLists_i 
        :: (States i m, A.TargetI l a)
        => Name l -> i -> [[a]] -> m (Sources i m l a)

fromLists_i nDst n xs
 = G.fromList n $ P.map (A.fromList nDst) xs
{-# INLINE fromLists_i #-}


-- | Drain a single source into a list of elements.
toList1_i 
        :: (States i m, A.BulkI l a)
        => Ix i -> Sources i m l a  -> m [a]
toList1_i i sources
 = do   chunks  <- G.toList1 i sources
        return  $ P.concat $ P.map A.toList chunks
{-# INLINE toList1_i #-}


-- | Drain a single source into a list of chunks.
toLists1_i
        :: (States i m, A.BulkI l a)
        => Ix i -> Sources i m l a -> m [[a]]
toLists1_i i sources
 = do   chunks  <- G.toList1 i sources
        return  $ P.map A.toList chunks
{-# INLINE toLists1_i #-}


-- | Split the given number of elements from the head of a source,
--   retrurning those elements in a list, and yielding a new source
--   for the rest.
--
--   * We pull /whole chunks/ from the source stream until we have
--     at least the desired number of elements. The leftover elements
--     in the final chunk are visible in the result `Sources`.
--
head_i  :: (States i m, A.Windowable l a, A.Index l ~ Int)
        => Int -> Sources i m l a -> Ix i -> m ([a], Sources i m l a)

head_i len s0 i
 = do   
        (s1, s2) <- G.connect_i s0

        let G.Sources n pull_chunk = s1

        -- Pull chunks from the source until we have enough elements to return.
        refsList  <- newRefs n Q.empty
        refsChunk <- newRefs n Nothing

        let loop_takeList1 !has !acc !mchunk
             | has >= len        
             = do writeRefs refsList  i acc
                  writeRefs refsChunk i mchunk

             | otherwise         
             = pull_chunk i eat_toList eject_toList
             where 
                   eat_toList x  
                    = loop_takeList1 
                        (has + A.length x) 
                        (acc Q.>< (Q.fromList $ A.toList x))
                        (Just x)

                   eject_toList  
                    = do writeRefs refsList  i acc
                         writeRefs refsChunk i mchunk

            {-# INLINE loop_takeList1 #-}

        loop_takeList1 0 Q.empty Nothing

        -- Split off the required number of elements.
        has     <- readRefs refsList  i
        mFinal  <- readRefs refsChunk i
        let (here, rest) = Q.splitAt len has

        -- As we've pulled whole chunks from the input stream,
        -- we now prepend the remaining ones back on.
        let start  =  Q.length has - Q.length rest
        let stash  = case mFinal of
                        Nothing -> []
                        Just c  -> [A.window start (Q.length rest) c]

        s2'        <- G.prependOn_i (\i' -> i' == i) stash s2
        return  (Q.toList here, s2')
{-# INLINE_FLOW head_i #-}


-- Finalizers -----------------------------------------------------------------
-- | Attach a finalizer to a bundle of sources.
--
--   For each stream in the bundle, the finalizer will be called the first
--   time a consumer of that stream tries to pull an element when no more
--   are available.
--
--   The provided finalizer will be run after any finalizers already
--   attached to the source.
--
finalize_i
        :: States i m
        => (Ix i -> m ())
        -> Sources i m l a -> m (Sources i m l a)
finalize_i = G.finalize_i
{-# INLINE finalize_i #-}


-- | Attach a finalizer to a bundle of sinks.
--
--   The finalizer will be called the first time the stream is ejected.
--
--   The provided finalizer will be run after any finalizers already
--   attached to the sink.
--
finalize_o
        :: States i m
        => (Ix i -> m ())
        -> Sinks i m l a -> m (Sinks i m l a)
finalize_o = G.finalize_o
{-# INLINE finalize_o #-}
