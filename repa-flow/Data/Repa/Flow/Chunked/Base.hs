
module Data.Repa.Flow.Chunked.Base
        ( Sources, Sinks
        , Flow
        , Data.Repa.Flow.Chunked.Base.fromList
        , fromLists
        , toList1
        , toLists1
        , head_i
        , finalize_i,    finalize_o)
where
import Data.Repa.Flow.States                    as F
import Data.Repa.Array.Generic                  as A
import Data.Repa.Array.Generic.Index            as A
import Data.Repa.Array.Meta.Window              as A
import Control.Monad
import qualified Data.Repa.Flow.Generic         as G
import qualified Data.Sequence                  as Q
import qualified Data.Foldable                  as D
import Prelude                                  as P
#include "repa-flow.h"


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
fromList  :: (States i m, A.TargetI l a)
          => Name l -> i -> [a] -> m (Sources i m l a)
fromList nDst n xs
 = G.fromList n [A.fromList nDst xs]
{-# INLINE fromList #-}


-- | Like `fromLists` but take a list of lists, where each of the inner
--   lists is packed into a single chunk.
fromLists :: (States i m, A.TargetI l a)
          => Name l -> i -> [[a]] -> m (Sources i m l a)

fromLists nDst n xs
 = G.fromList n $ P.map (A.fromList nDst) xs
{-# INLINE fromLists #-}


-- | Drain a single source into a list of elements.
toList1 :: (States i m, A.BulkI l a)
        => i -> Sources i m l a  -> m [a]
toList1 i sources
 = do   chunks  <- G.toList1 i sources
        return  $ P.concat $ P.map A.toList chunks
{-# INLINE toList1 #-}


-- | Drain a single source into a list of chunks.
toLists1 :: (States i m, A.BulkI l a)
         => i -> Sources i m l a -> m [[a]]
toLists1 i sources
 = do   chunks  <- G.toList1 i sources
        return  $ P.map A.toList chunks
{-# INLINE toLists1 #-}


-- | Split the given number of elements from the head of a source,
--   retrurning those elements in a list, and yielding a new source
--   for the rest.
--
--   * We pull /whole chunks/ from the source stream until we have
--     at least the desired number of elements. The leftover elements
--     in the final chunk are visible in the result `Sources`.
--
head_i  :: (States i m, A.Windowable l a, A.Index l ~ Int)
        => Int -> Sources i m l a -> i -> m ([a], Sources i m l a)

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
        let start  = Q.length has - Q.length rest
        let stash  = case mFinal of
                        Nothing -> []
                        Just c  -> [A.window start (Q.length rest) c]

        s2'        <- G.prependOn_i (\i' -> i' == i) stash s2
        return  (D.toList here, s2')
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
        => (i -> m ())
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
        => (i -> m ())
        -> Sinks i m l a -> m (Sinks i m l a)
finalize_o = G.finalize_o
{-# INLINE finalize_o #-}
