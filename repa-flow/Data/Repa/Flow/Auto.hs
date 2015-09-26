
-- | This module defines the default specialisation of flows that
--   appears in "Data.Repa.Flow". Each stream in the bundle is indexed
--   by a single integer, and stream state is stored using the IO monad.
--
module Data.Repa.Flow.Auto
        ( -- * Flow types
          Sources
        , Sinks
        , Flow
        , sourcesArity
        , sinksArity

        -- * Conversion
        -- ** List conversion
        , fromList,             fromLists
        , toList1,              toLists1

        -- ** Array conversion
        , fromArray,            fromArrays
        , toArray1,             toArrays1

        -- * Evaluation
        , drainS
        , drainP
        , consumeS

        -- * Flow Operators
        -- ** Replicating
        , replicates_i

        -- ** Mapping
        -- | If you want to work on a chunk at a time then use 
        --   `Data.Repa.Flow.Generic.map_i` and
        --   `Data.Repa.Flow.Generic.map_o` from "Data.Repa.Flow.Generic".
        , map_i,                map_o
        , zipWith_i

        -- ** Processing
        , process_i

        -- | Higher arity zipWith functions.
        , module Data.Repa.Flow.Auto.ZipWith

        -- ** Connecting
        , dup_oo
        , dup_io
        , dup_oi
        , connect_i

        -- ** Watching
        , watch_i,              watch_o
        , trigger_o

        -- ** Ignorance
        , ignore_o
        , abandon_o

        -- ** Splitting
        , head_i

        -- ** Concatenation
        , concat_i

        -- ** Selecting
        , select_i,             select_o
        , discard_i,            discard_o
        , mask_i,               mask_o

        -- ** Grouping
        , groups_i
        , groupsBy_i
        , GroupsDict

        -- ** Folding
        -- *** Complete
        , foldlS,               foldlAllS

        -- *** Segmented
        , folds_i,              FoldsDict
        , foldGroupsBy_i,       FoldGroupsDict

        -- * Finalizers
        , finalize_i,           finalize_o
        )
where
import Data.Repa.Flow.Auto.ZipWith
import Data.Repa.Flow.Auto.Select
import Data.Repa.Flow.Auto.Base
import Data.Repa.Array.Auto                    
        hiding (fromList, fromLists)

import Data.Repa.Array.Material.Auto                    (A(..), Name(..))
import Data.Repa.Fusion.Unpack                          as A
import qualified Data.Repa.Array.Meta.Window            as A
import qualified Data.Repa.Array.Material               as A
import qualified Data.Repa.Array.Generic                as A
import qualified Data.Repa.Array.Generic.Target         as A
import qualified Data.Repa.Flow.Chunked                 as C hiding (next)
import qualified Data.Repa.Flow.Generic                 as G hiding (next)
import Control.Monad
#include "repa-flow.h"


-- Evaluation -----------------------------------------------------------------
-- | Pull all available values from the sources and push them to the sinks.
--   Streams in the bundle are processed sequentially, from first to last.
--
--   * If the `Sources` and `Sinks` have different numbers of streams then
--     we only evaluate the common subset.
--
drainS   :: Sources a -> Sinks a -> IO ()
drainS = G.drainS
{-# INLINE drainS #-}


-- | Pull all available values from the sources and push them to the sinks,
--   in parallel. We fork a thread for each of the streams and evaluate
--   them all in parallel.
--
--   * If the `Sources` and `Sinks` have different numbers of streams then
--     we only evaluate the common subset.
--
drainP   :: Sources a -> Sinks a -> IO ()
drainP = G.drainP
{-# INLINE drainP #-}


-- | Pull all available values from the sources and pass them to the
--   given action.
--
consumeS :: A.Bulk A a
         => Sources a -> (Int -> a -> IO ()) -> IO ()
consumeS = C.consumeS
{-# INLINE consumeS #-}


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
---
--     TODO: make the finalizer run just the first time.
--
finalize_i
        :: (Int -> IO ())
        -> Sources a -> IO (Sources a)
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
---
--     TODO: make the finalizer run just the first time.
--
finalize_o
        :: (Int -> IO ())
        -> Sinks a   -> IO (Sinks a)
finalize_o f k 
        = G.finalize_o f k
{-# INLINE finalize_o #-}


-- Replicating ----------------------------------------------------------------
-- | Segmented replicate.
replicates_i 
        :: (Flow (Int, a), Build a at, Unpack (A.Buffer A a) att)
        => Sources (Int, a)     -- ^ Source of segment lengths and values.
        -> IO (Sources a)       
replicates_i = C.replicates_i A
{-# INLINE replicates_i #-}


-- Mapping --------------------------------------------------------------------
-- | Apply a function to all elements pulled from some sources.
map_i   :: (Flow a, Build b bt)
        => (a -> b) -> Sources a -> IO (Sources b)
map_i f s = C.smap_i (\_ x -> f x) s
{-# INLINE map_i #-}


-- | Apply a function to all elements pushed to some sinks.
map_o   :: (Flow a, Build b bt)
        => (a -> b) -> Sinks b   -> IO (Sinks a)
map_o f s = C.smap_o (\_ x -> f x) s
{-# INLINE map_o #-}


-- | Combine corresponding elements of two sources with the given function.
zipWith_i 
        :: (Flow a, Flow b, Build c bt)
        => (a -> b -> c) 
        -> Sources a -> Sources b 
        -> IO (Sources c)
zipWith_i f sa sb
        = C.szipWith_ii A (\_ a b -> f a b) sa sb
{-# INLINE zipWith_i #-}


-- Processing -----------------------------------------------------------------
-- | Apply a generic stream process to a bundle of sources.
process_i
        :: ( Flow a, Flow b, Build b bt
           , Unpack (Array b) bbt)
        => (s -> a -> (s, Array b))     -- ^ Worker function.
        -> s                            -- ^ Initial state.
        ->     Sources a                -- ^ Input sources.
        -> IO (Sources b)

process_i = C.process_i
{-# INLINE process_i #-}


-- Concatenation --------------------------------------------------------------
-- | Concatenate a flow of arrays into a flow of the elements.
concat_i
        :: (Flow a, Build a at, Unpack (Array a) att)
        => Sources (Array a)
        -> IO (Sources a)
concat_i ss
        = G.map_i (A.concat A) ss
{-# INLINE concat_i #-}


-- Connecting -----------------------------------------------------------------
-- | Send the same data to two consumers.
--
--   Given two argument sinks, yield a result sink.
--   Pushing to the result sink causes the same element to be pushed to both
--   argument sinks. 
dup_oo  :: Sinks a -> Sinks a -> IO (Sinks a)
dup_oo = G.dup_oo
{-# INLINE dup_oo #-}


-- | Send the same data to two consumers.
--  
--   Given an argument source and argument sink, yield a result source.
--   Pulling an element from the result source pulls from the argument source,
--   and pushes that element to the sink, as well as returning it via the
--   result source.
--   
dup_io  :: Sources a -> Sinks a -> IO (Sources a)
dup_io = G.dup_io
{-# INLINE dup_io #-}


-- | Send the same data to two consumers.
--
--   Like `dup_io` but with the arguments flipped.
--
dup_oi  :: Sinks a -> Sources a -> IO (Sources a)
dup_oi = G.dup_oi
{-# INLINE dup_oi #-}


-- | Connect an argument source to two result sources.
--
--   Pulling from either result source pulls from the argument source.
--   Each result source only gets the elements pulled at the time, 
--   so if one side pulls all the elements the other side won't get any.
--
connect_i :: Sources a -> IO (Sources a, Sources a)
connect_i = G.connect_i
{-# INLINE connect_i #-}


-- Watching -------------------------------------------------------------------
-- | Hook a worker function to some sources, which will be passed every
--   chunk that is pulled from each source.
--
--   * The worker is also passed the source index of the chunk that was pulled.
--
watch_i :: (Int -> A.Array A a -> IO ()) 
        -> Sources a  -> IO (Sources a)
watch_i f s = G.watch_i f s
{-# INLINE watch_i #-}


-- | Hook a worker function to some sinks, which will be passed every 
--   chunk that is pushed to each sink.
--
--   * The worker is also passed the source index of the chunk that was pushed.
--
watch_o :: (Int -> A.Array A a -> IO ())
        -> Sinks a    -> IO (Sinks a)
watch_o f k = G.watch_o f k
{-# INLINE watch_o #-}


-- | Create a bundle of sinks of the given arity that pass incoming chunks
--   to a worker function. 
--
--   * This is like `watch_o`, except that the incoming chunks are discarded
--     after they are passed to the worker function
--
trigger_o :: Int -> (Int -> A.Array A a -> IO ()) 
          -> IO (Sinks a)
trigger_o arity f 
        = G.trigger_o arity f
{-# INLINE trigger_o #-}


-- Ignorance ------------------------------------------------------------------
-- | Create a bundle of sinks of the given arity that drop all data on the
--   floor. 
--
--   * Haskell debugging thunks attached to the chunks will be
--     demanded, but thunks attached to elements may not be -- depending on
--     whether the chunk representation is strict in the elements.
--
ignore_o :: Int -> IO (Sinks a)
ignore_o  = G.ignore_o
{-# INLINE ignore_o #-}


-- | Create a bundle of sinks of the given arity that drop all data on the
--   floor.
--
--   * As opposed to `ignore_o` the sinks are non-strict in the chunks.
--   * Haskell debugging thunks attached to the chunks will *not* be 
--     demanded.
--
abandon_o :: Int -> IO (Sinks a)
abandon_o = G.abandon_o
{-# INLINE abandon_o #-}



-- Splitting ------------------------------------------------------------------
-- | Given a source index and a length, split the a list of that
--   length from the front of the source. Yields a new source for the
--   remaining elements.
--
--   * We pull /whole chunks/ from the source stream until we have
--     at least the desired number of elements. The leftover elements
--     in the final chunk are visible in the result `Sources`.
--
head_i  :: Flow a
        => Int -> Int -> Sources a -> IO (Maybe ([a], Sources a))
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
-- > F.toList1 0 =<< F.groups_i =<< F.fromList 1 "waabbbblle"
-- [(\'w\',1),(\'a\',2),(\'b\',4),(\'l\',2),(\'e\',1)]
-- @
--
groups_i
        :: (GroupsDict a u1 u2, Eq a)
        => Sources a       -- ^ Input elements.
        -> IO (Sources (a, Int)) 
                                -- ^ Starting element and length of groups.
groups_i s
        = groupsBy_i (==) s
{-# INLINE groups_i #-}


-- | Like `groupsBy`, but take a function to determine whether two consecutive
--   values should be in the same group.
groupsBy_i
        :: GroupsDict a u1 u2
        => (a -> a -> Bool)     -- ^ Fn to check if consecutive elements
                                --   are in the same group.
        -> Sources a       -- ^ Input elements.
        -> IO (Sources (a, Int)) 
                                -- ^ Starting element and length of groups.
groupsBy_i f s
        =   G.map_i (A.convert A)
        =<< C.groupsBy_i A A f s
{-# INLINE groupsBy_i #-}


-- | Dictionaries needed to perform a grouping.
type GroupsDict a u1 u2
        = C.GroupsDict Int IO A A u1 A u2 a


-- Folding --------------------------------------------------------------------
-- | Fold all the elements of each stream in a bundle, one stream after the
--   other, returning an array of fold results.
foldlS
        :: (Flow b, Build a at)
        => (a -> b -> a)                -- ^ Combining funtion.
        -> a                            -- ^ Starting value.
        -> Sources b                    -- ^ Input elements to fold.
        -> IO (A.Array A a)

foldlS f z ss
        = C.foldlS A f z ss
{-# INLINE foldlS #-}


-- | Fold all the elements of each stream in a bundle, one stream after the
--   other, returning an array of fold results.
foldlAllS
        :: (Flow b)
        => (a -> b -> a)                -- ^ Combining funtion.
        -> a                            -- ^ Starting value.
        -> Sources b                    -- ^ Input elements to fold.
        -> IO a

foldlAllS f z ss
        = C.foldlAllS f z ss
{-# INLINE foldlAllS #-}


-- | Given streams of lengths and values, perform a segmented fold where
--   fold segments of values of the corresponding lengths are folded 
--   together.
--
-- @
-- > sSegs <- F.fromList 1 [(\'a\', 1), (\'b\', 2), (\'c\', 4), (\'d\', 0), (\'e\', 1), (\'f\', 5 :: Int)]
-- > sVals <- F.fromList 1 [10, 20, 30, 40, 50, 60, 70, 80, 90 :: Int]
--
-- > F.toList1 0 =<< F.folds_i (+) 0 sSegs sVals
-- [(\'a\',10),(\'b\',50),(\'c\',220),(\'d\',0),(\'e\',80)]
-- @
--
--   If not enough input elements are available to fold a complete segment
--   then no output is produced for that segment. However, trailing zero
--   length segments still produce the initial value for the fold.
--
-- @
-- > sSegs <- F.fromList 1 [(\'a\', 1), (\'b\', 2), (\'c\', 0), (\'d\', 0), (\'e\', 0 :: Int)]
-- > sVals <- F.fromList 1 [10, 20, 30 :: Int]
--
-- > F.toList1 0 =<< F.folds_i (*) 1 sSegs sVals
-- [(\'a\',10),(\'b\',600),(\'c\',1),(\'d\',1),(\'e\',1)]
-- @
--
folds_i :: FoldsDict n a b u1 u2 u3 u4
        => (a -> b -> b)          -- ^ Worker function.
        -> b                      -- ^ Initial state when folding each segment.
        -> Sources (n, Int)       -- ^ Segment lengths.
        -> Sources a              -- ^ Input elements to fold.
        -> IO (Sources  (n, b))   -- ^ Result elements.

folds_i f z sLen sVal
        =   G.map_i (A.convert A)
        =<< C.folds_i A A f z sLen sVal
{-# INLINE folds_i #-}

-- | Dictionaries needed to perform a segmented fold.
type FoldsDict n a b u1 u2 u3 u4
        = C.FoldsDict Int IO A u1 A u2 A u3 A u4 n a b


-- | Combination of `groupsBy_i` and `folds_i`. We determine the the segment
--   lengths while performing the folds.
-- 
--   Note that a SQL-like groupby aggregations can be performed using this 
--   function, provided the data is pre-sorted on the group key. For example,
--   we can take the average of some groups of values:
--
-- @
-- > sKeys <- F.fromList 1 "waaaabllle"
-- > sVals <- F.fromList 1 [10, 20, 30, 40, 50, 60, 70, 80, 90, 100 :: Double]
-- 
-- > sResult \<-  F.map_i (\\(key, (acc, n)) -\> (key, acc / n))
--           =\<\< F.foldGroupsBy_i (==) (\\x (acc, n) -> (acc + x, n + 1)) (0, 0) sKeys sVals
--
-- > F.toList1 0 sResult
-- [10.0,35.0,60.0,80.0,100.0]
-- @
--
foldGroupsBy_i
        :: (FoldGroupsDict n a b u1 u2)
        => (n -> n -> Bool)     -- ^ Fn to check if consecutive elements
                                --   are in the same group.
        -> (a -> b -> b)        -- ^ Worker function for the fold.
        -> b                    -- ^ Initial when folding each segment.
        -> Sources n            -- ^ Names that determine groups.
        -> Sources a            -- ^ Values to fold.
        -> IO (Sources (n, b))

foldGroupsBy_i pGroup f z sNames sVals
 = do   segLens <- groupsBy_i pGroup sNames
        folds_i f z segLens sVals
{-# INLINE foldGroupsBy_i #-}
 

type FoldGroupsDict  n a b u1 u2
      = ( A.BulkI    A n
        , A.Material A a
        , A.Material A n
        , A.Material A b
        , Unpack  (A.Buffer A n) u1
        , Unpack  (A.Buffer A b) u2)
