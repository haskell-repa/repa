
-- | This module defines the default specialisation of flows that
--   appears in "Data.Repa.Flow". Each stream in the bundle is indexed
--   by a single integer, and stream state is stored using the IO monad.
--
module Data.Repa.Flow.Default
        ( -- * Flow types
          Sources
        , Sinks
        , Flow
        , sourcesArity
        , sinksArity

        -- * States and Arrays
        , module Data.Repa.Flow.States
        , module Data.Repa.Eval.Array
        , module Data.Repa.Array
        , module Data.Repa.Array.Material

        -- * Evaluation
        , drainS
        , drainP

        -- * Conversion
        , fromList,             fromLists
        , toList1,              toLists1

        -- * Finalizers
        , finalize_i,           finalize_o

        -- * Flow Operators
        -- ** Mapping
        -- | If you want to work on a chunk at a time then use 
        --   `Data.Repa.Flow.Generic.map_i` and
        --   `Data.Repa.Flow.Generic.map_o` from "Data.Repa.Flow.Generic".
        , map_i,                map_o

        -- ** Connecting
        , dup_oo
        , dup_io
        , dup_oi
        , connect_i

        -- ** Watching
        , watch_i,              watch_o
        , trigger_o

        -- ** Ignorance
        , discard_o
        , ignore_o

        -- ** Splitting
        , head_i

        -- ** Grouping
        , groups_i
        , groupsBy_i
        , GroupsDict

        -- ** Folding
        , foldlS_i,             foldlAllS_i
        , folds_i,              FoldsDict
        , foldGroupsBy_i,       FoldGroupsDict)
where
import Data.Repa.Flow.States
import Data.Repa.Eval.Array
import Data.Repa.Eval.Array              as A

import Data.Repa.Array                   
        hiding (FoldsDict, GroupsDict, Index, fromList)

import Data.Repa.Array                   as A 
        hiding (FoldsDict, GroupsDict, fromList)

import Data.Repa.Array.Material          hiding (fromLists)
import Data.Repa.Fusion.Unpack           as A
import qualified Data.Repa.Flow.Chunked  as C hiding (next)
import qualified Data.Repa.Flow.Generic  as G hiding (next)
import Control.Monad
#include "repa-flow.h"


-- | A bundle of stream sources, where the elements of the stream
--   are chunked into arrays.
--
--   The chunks have some `Layout` @l@ and contain elements of type @a@.
--   See "Data.Repa.Array" for the available layouts.
type Sources l a = C.Sources Int IO l a


-- | A bundle of stream sinks,   where the elements of the stream
--   are chunked into arrays.
--
type Sinks   l a = C.Sinks Int IO l a


-- | Yield the number of streams in the bundle.
sourcesArity :: Sources l a -> Int
sourcesArity = G.sourcesArity


-- | Yield the number of streams in the bundle.
sinksArity :: Sinks l a -> Int
sinksArity = G.sinksArity


-- | Shorthand for common type classes.
type Flow    l a = C.Flow  Int IO l a


-- Evaluation -----------------------------------------------------------------
-- | Pull all available values from the sources and push them to the sinks.
--   Streams in the bundle are processed sequentially, from first to last.
--
--   * If the `Sources` and `Sinks` have different numbers of streams then
--     we only evaluate the common subset.
--
drainS   :: Sources l a -> Sinks l a -> IO ()
drainS = G.drainS
{-# INLINE drainS #-}


-- | Pull all available values from the sources and push them to the sinks,
--   in parallel. We fork a thread for each of the streams and evaluate
--   them all in parallel.
--
--   * If the `Sources` and `Sinks` have different numbers of streams then
--     we only evaluate the common subset.
--
drainP   :: Sources l a -> Sinks l a -> IO ()
drainP = G.drainP
{-# INLINE drainP #-}


-- Conversion -----------------------------------------------------------------
-- | Given an arity and a list of elements, yield sources that each produce all
--   the elements. 
--
--   * All elements are stuffed into a single chunk, and each stream is given
--     the same chunk.
--
fromList :: A.TargetI l a
         => Name l -> Int -> [a] -> IO (Sources l a)
fromList l xs = C.fromList l xs
{-# INLINE fromList #-}


-- | Like `fromLists_i` but take a list of lists. Each each of the inner
--   lists is packed into a single chunk.
fromLists :: A.TargetI l a
          => Name l -> Int -> [[a]] -> IO (Sources l a)
fromLists nDst xss = C.fromLists nDst xss
{-# INLINE fromLists #-}


-- | Drain a single source from a bundle into a list of elements.
toList1   :: A.BulkI l a
          => Int -> Sources l a -> IO [a]
toList1 ix s  
 | ix >= G.sourcesArity s = return []
 | otherwise              = C.toList1 ix s 
{-# INLINE toList1 #-}


-- | Drain a single source from a bundle into a list of chunks.
toLists1  :: A.BulkI l a
          => Int -> Sources l a -> IO [[a]]
toLists1 ix s
 | ix >= G.sourcesArity s = return []
 | otherwise              = C.toLists1 ix s 
{-# INLINE toLists1 #-}


-- Finalizers -----------------------------------------------------------------
-- | Attach a finalizer to some sources.
--
--   * For a given source, the finalizer will be called the first time a
--     consumer of that source tries to pull an element when no more
--     are available. 
--
--   * The finalizer is given the index of the source that ended.
--
--   * The finalizer will be run after any finalizers already attached
--     to the source.
--
--     TODO: make the finalizer run just the first time.
--
finalize_i
        :: (Int -> IO ())
        -> Sources l a -> IO (Sources l a)
finalize_i f s 
        = G.finalize_i f s
{-# INLINE finalize_i #-}


-- | Attach a finalizer to some sinks.
--
--   * For a given sink, the finalizer will be called the first time
--     that sink is ejected.
--      
--   * The finalizer is given the index of the sink that was ejected.
--
--   * The finalizer will be run after any finalizers already attached
--     to the sink.
--
--     TODO: make the finalizer run just the first time.
--
finalize_o
        :: (Int -> IO ())
        -> Sinks l a   -> IO (Sinks l a)
finalize_o f k 
        = G.finalize_o f k
{-# INLINE finalize_o #-}


-- Mapping --------------------------------------------------------------------
-- | Apply a function to all elements pulled from some sources.
map_i   :: (Flow l1 a, A.TargetI l2 b)
        => Name l2 -> (a -> b) -> Sources l1 a -> IO (Sources l2 b)
map_i _ f s = C.smap_i (\_ x -> f x) s
{-# INLINE map_i #-}


-- | Apply a function to all elements pushed to some sinks.
map_o   :: (Flow l1 a, A.TargetI l2 b)
        => Name l1 -> (a -> b) -> Sinks l2 b   -> IO (Sinks   l1 a)
map_o _ f s = C.smap_o (\_ x -> f x) s
{-# INLINE map_o #-}


-- Connecting -----------------------------------------------------------------
-- | Send the same data to two consumers.
--
--   Given two argument sinks, yield a result sink.
--   Pushing to the result sink causes the same element to be pushed to both
--   argument sinks. 
dup_oo  :: Sinks l a -> Sinks l a -> IO (Sinks l a)
dup_oo = G.dup_oo
{-# INLINE dup_oo #-}


-- | Send the same data to two consumers.
--  
--   Given an argument source and argument sink, yield a result source.
--   Pulling an element from the result source pulls from the argument source,
--   and pushes that element to the sink, as well as returning it via the
--   result source.
--   
dup_io  :: Sources l a -> Sinks l a -> IO (Sources l a)
dup_io = G.dup_io
{-# INLINE dup_io #-}


-- | Send the same data to two consumers.
--
--   Like `dup_io` but with the arguments flipped.
--
dup_oi  :: Sinks l a -> Sources l a -> IO (Sources l a)
dup_oi = G.dup_oi
{-# INLINE dup_oi #-}


-- | Connect an argument source to two result sources.
--
--   Pulling from either result source pulls from the argument source.
--   Each result source only gets the elements pulled at the time, 
--   so if one side pulls all the elements the other side won't get any.
--
connect_i :: Sources l a -> IO (Sources l a, Sources l a)
connect_i = G.connect_i
{-# INLINE connect_i #-}


-- Watching -------------------------------------------------------------------
-- | Hook a worker function to some sources, which will be passed every
--   chunk that is pulled from each source.
--
--   * The worker is also passed the source index of the chunk that was pulled.
--
watch_i :: (Int -> Array l a -> IO ()) 
        -> Sources l a  -> IO (Sources l a)
watch_i f s = G.watch_i f s
{-# INLINE watch_i #-}


-- | Hook a worker function to some sinks, which will be passed every 
--   chunk that is pushed to each sink.
--
--   * The worker is also passed the source index of the chunk that was pushed.
--
watch_o :: (Int -> Array l a -> IO ())
        -> Sinks l a    -> IO (Sinks l a)
watch_o f k = G.watch_o f k
{-# INLINE watch_o #-}


-- | Create a bundle of sinks of the given arity that pass incoming chunks
--   to a worker function. 
--
--   * This is like `watch_o`, except that the incoming chunks are discarded
--     after they are passed to the worker function
--
trigger_o :: Int -> (Int -> Array l a -> IO ()) 
          -> IO (Sinks l a)
trigger_o arity f 
        = G.trigger_o arity f
{-# INLINE trigger_o #-}


-- Ignorance ------------------------------------------------------------------
-- | Create a bundle of sinks of the given arity that drop all data on the
--   floor.
--
--   * The sinks is strict in the *chunks*, so they are demanded before being
--     discarded. 
--   * Haskell debugging thunks attached to the chunks will be
--     demanded, but thunks attached to elements may not be -- depending on
--     whether the chunk representation is strict in the elements.
--
discard_o :: Int -> IO (Sinks l a)
discard_o = G.discard_o
{-# INLINE discard_o #-}


-- | Create a bundle of sinks of the given arity that drop all data on the
--   floor. 
--
--   * As opposed to `discard_o` the sinks are non-strict in the chunks.
--   * Haskell debugging thunks attached to the chunks will *not* be 
--     demanded.
--
ignore_o :: Int -> IO (Sinks l a)
ignore_o  = G.ignore_o
{-# INLINE ignore_o #-}


-- Splitting ------------------------------------------------------------------
-- | Given a source index and a length, split the a list of that
--   length from the front of the source. Yields a new source for the
--   remaining elements.
--
--   * We pull /whole chunks/ from the source stream until we have
--     at least the desired number of elements. The leftover elements
--     in the final chunk are visible in the result `Sources`.
--
head_i  :: (A.Windowable l a, A.Index l ~ Int)
        => Int -> Int -> Sources l a -> IO (Maybe ([a], Sources l a))
head_i ix len s
 | ix >= G.sourcesArity s = return Nothing
 | otherwise             
 = liftM Just $ C.head_i len s ix
{-# INLINE head_i #-}


-- Grouping -------------------------------------------------------------------
-- | Scan through some sources to find runs of matching elements, 
--   and count the lengths of those runs.
--
-- @  
-- > import Data.Repa.Flow
-- > toList1 0 =<< groups_i U U =<< fromList U 1 "waabbbblle"
-- Just [(\'w\',1),(\'a\',2),(\'b\',4),(\'l\',2),(\'e\',1)]
-- @
--
groups_i
        :: (GroupsDict lVal lGrp tGrp lLen tLen a, Eq a)
        => Name lGrp            -- ^ Layout of result groups.
        -> Name lLen            -- ^ Layout of result lengths.
        -> Sources lVal a       -- ^ Input elements.
        -> IO (Sources (T2 lGrp lLen) (a, Int)) 
                                -- ^ Starting element and length of groups.
groups_i nGrp nLen s
        = groupsBy_i nGrp nLen (==) s
{-# INLINE groups_i #-}


-- | Like `groupsBy`, but take a function to determine whether two consecutive
--   values should be in the same group.
groupsBy_i
        :: GroupsDict lVal lGrp tGrp lLen tLen a
        => Name lGrp            -- ^ Layout of result groups.
        -> Name lLen            -- ^ Layout of result lengths.
        -> (a -> a -> Bool)     -- ^ Fn to check if consecutive elements
                                --   are in the same group.
        -> Sources lVal a       -- ^ Input elements.
        -> IO (Sources (T2 lGrp lLen) (a, Int)) 
                                -- ^ Starting element and length of groups.
groupsBy_i nGrp nLen f s
        = C.groupsBy_i nGrp nLen f s
{-# INLINE groupsBy_i #-}


-- | Dictionaries needed to perform a grouping.
type GroupsDict lVal lGrp tGrp lLen tLen a
        = C.GroupsDict Int IO lVal lGrp tGrp lLen tLen a


-- Folding --------------------------------------------------------------------
-- | Fold all the elements of each stream in a bundle, one stream after the
--   other, returning an array of fold results.
foldlS_i  
        :: ( A.Target lDst a, A.Index lDst ~ Int
           , A.BulkI  lSrc b)
        => A.Name lDst                  -- ^ Layout for result.
        -> (a -> b -> a)                -- ^ Combining funtion.
        -> a                            -- ^ Starting value.
        -> Sources lSrc b               -- ^ Input elements to fold.
        -> IO (A.Array lDst a)

foldlS_i n f z ss
        = C.foldlS_i n f z ss
{-# INLINE foldlS_i #-}


-- | Fold all the elements of each stream in a bundle, one stream after the
--   other, returning an array of fold results.
foldlAllS_i  
        :: A.BulkI lSrc b
        => (a -> b -> a)                -- ^ Combining funtion.
        -> a                            -- ^ Starting value.
        -> Sources lSrc b               -- ^ Input elements to fold.
        -> IO a

foldlAllS_i f z ss
        = C.foldlAllS_i f z ss
{-# INLINE foldlAllS_i #-}


-- | Given streams of lengths and values, perform a segmented fold where
--   fold segments of values of the corresponding lengths are folded 
--   together.
--
-- @
-- > import Data.Repa.Flow
-- > sSegs <- fromList U 1 [(\'a\', 1), (\'b\', 2), (\'c\', 4), (\'d\', 0), (\'e\', 1), (\'f\', 5 :: Int)]
-- > sVals <- fromList U 1 [10, 20, 30, 40, 50, 60, 70, 80, 90 :: Int]
-- > toList1 0 =<< folds_i U U (+) 0 sSegs sVals
-- Just [(\'a\',10),(\'b\',50),(\'c\',220),(\'d\',0),(\'e\',80)]
-- @
--
--   If not enough input elements are available to fold a complete segment
--   then no output is produced for that segment. However, trailing zero
--   length segments still produce the initial value for the fold.
--
-- @
-- > import Data.Repa.Flow
-- > sSegs <- fromList U 1 [(\'a\', 1), (\'b\', 2), (\'c\', 0), (\'d\', 0), (\'e\', 0 :: Int)]
-- > sVals <- fromList U 1 [10, 20, 30 :: Int]
-- > toList1 0 =<< folds_i U U (*) 1 sSegs sVals
-- Just [(\'a\',10),(\'b\',600),(\'c\',1),(\'d\',1),(\'e\',1)]
-- @
--
folds_i :: (FoldsDict lSeg tSeg lElt tElt lGrp tGrp lRes tRes n a b)
        => Name lGrp              -- ^ Layout for group names.
        -> Name lRes              -- ^ Layout for fold results.
        -> (a -> b -> b)          -- ^ Worker function.
        -> b                      -- ^ Initial state when folding each segment.
        -> Sources lSeg (n, Int)  -- ^ Segment lengths.
        -> Sources lElt a         -- ^ Input elements to fold.
        -> IO (Sources (T2 lGrp lRes) (n, b)) -- ^ Result elements.

folds_i nGrp nRes f z sLen sVal
        = C.folds_i nGrp nRes f z sLen sVal
{-# INLINE folds_i #-}

-- | Dictionaries needed to perform a segmented fold.
type FoldsDict lSeg tSeg lElt tElt lGrp tGrp lRes tRes n a b
        = C.FoldsDict Int IO lSeg tSeg lElt tElt lGrp tGrp lRes tRes n a b


-- | Combination of `groupsBy_i` and `folds_i`. We determine the the segment
--   lengths while performing the folds.
-- 
--   Note that a SQL-like groupby aggregations can be performed using this 
--   function, provided the data is pre-sorted on the group key. For example,
--   we can take the average of some groups of values:
--
-- @
-- > import Data.Repa.Flow
-- > sKeys   <-  fromList U 1 "waaaabllle"
-- > sVals   <-  fromList U 1 [10, 20, 30, 40, 50, 60, 70, 80, 90, 100 :: Double]
-- 
-- > sResult \<-  map_i U (\\(key, (acc, n)) -\> (key, acc / n))
--           =\<\< foldGroupsBy_i U U (==) (\\x (acc, n) -> (acc + x, n + 1)) (0, 0) sKeys sVals
--
-- > toList1 0 sResult
-- Just [10.0,35.0,60.0,80.0,100.0]
-- @
--
foldGroupsBy_i
        :: ( FoldGroupsDict lSeg tSeg lVal tVal lGrp tGrp lRes tRes n a b)
        => Name lGrp            -- ^ Layout for group names.
        -> Name lRes            -- ^ Layout for fold results.
        -> (n -> n -> Bool)     -- ^ Fn to check if consecutive elements
                                --   are in the same group.
        -> (a -> b -> b)        -- ^ Worker function for the fold.
        -> b                    -- ^ Initial when folding each segment.
        -> Sources lSeg n       -- ^ Names that determine groups.
        -> Sources lVal a       -- ^ Values to fold.
        -> IO (Sources (T2 lGrp lRes) (n, b))

foldGroupsBy_i nGrp nRes pGroup f z sNames sVals
 = do   segLens <- groupsBy_i nGrp U pGroup sNames
        folds_i nGrp nRes f z segLens sVals
{-# INLINE foldGroupsBy_i #-}
 

type FoldGroupsDict  lSeg tSeg lElt tElt lGrp tGrp lRes tRes n a b
      = ( A.BulkI    lSeg n
        , A.Material lElt a, A.Index lElt ~ Int
        , A.Material lGrp n, A.Index lGrp ~ Int
        , A.Material lRes b, A.Index lRes ~ Int
        , Unpack (IOBuffer lGrp n) tGrp
        , Unpack (IOBuffer lRes b) tRes)
