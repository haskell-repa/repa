{-# LANGUAGE UndecidableInstances #-}

-- | Data Parallel Data Flows.
--
--   NOTE: Althogh @repa-flow@ can be used productively in the ghci REPL, 
--   performance won't be great because you will be running unspecialised,
--   polymorphic code. For best results you should write a complete
--   program and compile it with @ghc -fllvm -O2 Main.hs@
--
-- * Getting Started
--
--   Open a stream for a single file using 64k chunks, with the chunks
--   ending on newline characters. Say "argh" if we find a line longer than 64k.
--
-- @
-- > import Data.Repa.Flow
-- > ws <- fileSourcesLines [\"\/usr\/share\/dict\/words\"] (64 * 1024) (error \"argh\")
-- @
--
--   Show the first 40 characters of the first chunk in source 0.
--
-- @ 
-- > next 0 40 ws
-- Just \"A\\nA\'s\\nAA\'s\\nAB\'s\\nABM\'s\\nAC\'s\\nACTH\'s\\nAI\'s\\nA\"
-- @
--
--   Show the first 40 characters of the next chunk.
--
-- @
-- > next 0 40 ws
-- Just \"Jubal\\nJudah\\nJudaic\\nJudaism\\nJudaism\'s\\nJud\"
-- @
--
module Data.Repa.Flow
        ( module Data.Repa.Flow.States
        , module Data.Repa.Array
        , module Data.Repa.Array.Material.Safe
        , module Data.Repa.Eval.Array

        -- * Flow types
        , Sources, Sinks
        , Flow

        -- * Evaluation
        , drain

        -- * Conversion
        , fromList,     fromLists
        , toList1,      toLists1

        -- * Finalizers
        , finalize_i,   finalize_o

        -- * Flow Operators
        -- ** Mapping
        , map_i,        map_o
        , mapChunks_i,  mapChunks_o
        , smapChunks_i, smapChunks_o

        -- ** Connecting
        , dup_oo
        , dup_io
        , dup_oi
        , connect_i

        -- ** Watching
        , watch_i,      watch_o
        , trigger_o

        -- ** Ignorance
        , discard_o
        , ignore_o

        -- ** Splitting
        , head_i

        -- ** Grouping
        , groups_i
        , groupsBy_i

        -- ** Folding
        , folds_i
        , foldGroupsBy_i
        , FoldsWorthy)
where
import Data.Repa.Flow.States
import Data.Repa.Eval.Array
import Data.Repa.Array.Material.Safe
import Data.Repa.Eval.Array                      as A
import Data.Repa.Array                           hiding (fromList)
import Data.Repa.Array                           as A hiding (fromList)
import qualified Data.Repa.Array.Material.Unsafe        as U
import qualified Data.Repa.Flow.Chunked          as C hiding (next)
import qualified Data.Repa.Flow.Generic          as G hiding (next)
import Control.Monad
#include "repa-stream.h"


-- | A bundle of data sources, where the elements are chunked into arrays.
--
--   The chunks have some representation @r@ and contain elements of type @a@.
type Sources r a = C.Sources Int IO r a


-- | A bundle of data sinks,   where the elements are chunked into arrays.
--
--   The chunks have some representation @r@ and contain elements of type @a@.
type Sinks   r a = C.Sinks   Int IO r a


-- | Shorthand for common type classes.
type Flow    r a = C.Flow    Int IO r a


-- Evaluation -----------------------------------------------------------------
-- | Pull all available values from the sources and push them to the sinks.
drain   :: Sources r a -> Sinks r a -> IO ()
drain = G.drain
{-# INLINE drain #-}


-- Conversion -----------------------------------------------------------------
-- | Given an arity and a list of elements, yield sources that each produce all
--   the elements. 
--
--   * All elements are stuffed into a single chunk, and each stream is given
--     the same chunk.
--
fromList :: A.Target r a t
         => r -> Int -> [a] -> IO (Sources r a)
fromList r xs = C.fromList_i r xs
{-# INLINE fromList #-}


-- | Like `fromLists_i` but take a list of lists. Each each of the inner
--   lists is packed into a single chunk.
fromLists :: A.Target r a t
          => r -> Int -> [[a]] -> IO (Sources r a)
fromLists r xss = C.fromLists_i r xss
{-# INLINE fromLists #-}


-- | Drain a single source from a bundle into a list of elements.
toList1   :: A.Bulk r DIM1 a
          => Int -> Sources r a -> IO (Maybe [a])
toList1 ix s  
 | ix >= G.sourceArity s = return Nothing
 | otherwise             
 = liftM Just $ C.toList1_i (IIx ix (G.sourceArity s)) s 
{-# INLINE toList1 #-}


-- | Drain a single source from a bundle into a list of chunks.
toLists1  :: A.Bulk r DIM1 a
          =>  Int -> Sources r a -> IO (Maybe [[a]])
toLists1 ix s
 | ix >= G.sourceArity s = return Nothing
 | otherwise             
 = liftM Just $ C.toLists1_i (IIx ix (G.sourceArity s)) s 
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
        -> Sources r a -> IO (Sources r a)
finalize_i f s 
        = G.finalize_i (\(IIx i _) -> f i) s
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
        -> Sinks r a   -> IO (Sinks r a)
finalize_o f k 
        = G.finalize_o (\(IIx i _) -> f i) k
{-# INLINE finalize_o #-}


-- Mapping --------------------------------------------------------------------
-- | Apply a function to all elements pulled from some sources.
map_i   :: (Flow r1 a, A.Target r2 b t)
        => r2 -> (a -> b) -> Sources r1 a -> IO (Sources r2 b)
map_i _ f s = C.map_i f s
{-# INLINE map_i #-}


-- | Apply a function to all elements pushed to some sinks.
map_o   :: (Flow r1 a, A.Target r2 b t)
        => r1 -> (a -> b) -> Sinks r2 b   -> IO (Sinks   r1 a)
map_o _ f s = C.map_o f s
{-# INLINE map_o #-}


-- | Apply a function to all elements pulled from some sources,
--   a chunk at a time.
mapChunks_i  
        :: (Vector r1 a -> Vector r2 b)
        -> Sources r1 a -> IO (Sources r2 b)
mapChunks_i f s 
        = G.smap_i (\_ c -> f c) s
{-# INLINE mapChunks_i #-}


-- | Apply a function to all elements pushed to some sinks,
--   a chunk at a time.
mapChunks_o  
        :: (Vector r1 a -> Vector r2 b)
        -> Sinks r2 b -> IO (Sinks r1 a)
mapChunks_o f s 
        = G.smap_o (\_ c -> f c) s
{-# INLINE mapChunks_o #-}


-- | Like `mapChunks_i`, except that the worker function is also given
--   the source index.
smapChunks_i  
        :: (Int -> Vector r1 a -> Vector r2 b)
        -> Sources r1 a -> IO (Sources r2 b)
smapChunks_i f s
        = G.smap_i (\(IIx i _) vec -> f i vec) s
{-# INLINE smapChunks_i #-}


-- | Like `mapChunks_o`, except that the worker function is also given
--   the sink index.
smapChunks_o  
        :: (Int -> Vector r1 a -> Vector r2 b)
        -> Sinks r2 b -> IO (Sinks r1 a)
smapChunks_o f k
        = G.smap_o (\(IIx i _) vec -> f i vec) k
{-# INLINE smapChunks_o #-}


-- Connecting -----------------------------------------------------------------
-- | Send the same data to two consumers.
--
--   Given two argument sinks, yield a result sink.
--   Pushing to the result sink causes the same element to be pushed to both
--   argument sinks. 
dup_oo  :: Sinks r a -> Sinks r a -> IO (Sinks r a)
dup_oo = G.dup_oo
{-# INLINE dup_oo #-}


-- | Send the same data to two consumers.
--  
--   Given an argument source and argument sink, yield a result source.
--   Pulling an element from the result source pulls from the argument source,
--   and pushes that element to the sink, as well as returning it via the
--   result source.
--   
dup_io  :: Sources r a -> Sinks r a -> IO (Sources r a)
dup_io = G.dup_io
{-# INLINE dup_io #-}


-- | Send the same data to two consumers.
--
--   Like `dup_io` but with the arguments flipped.
--
dup_oi  :: Sinks r a -> Sources r a -> IO (Sources r a)
dup_oi = G.dup_oi
{-# INLINE dup_oi #-}


-- | Connect an argument source to two result sources.
--
--   Pulling from either result source pulls from the argument source.
--   Each result source only gets the elements pulled at the time, 
--   so if one side pulls all the elements the other side won't get any.
--
connect_i :: Sources r a -> IO (Sources r a, Sources r a)
connect_i = G.connect_i
{-# INLINE connect_i #-}


-- Watching -------------------------------------------------------------------
-- | Hook a worker function to some sources, which will be passed every
--   chunk that is pulled from each source.
--
--   * The worker is also passed the source index of the chunk that was pulled.
--
watch_i :: (Int -> Vector r a -> IO ()) 
        -> Sources r a  -> IO (Sources r a)
watch_i f s = G.watch_i (\(IIx i _) vec -> f i vec) s
{-# INLINE watch_i #-}


-- | Hook a worker function to some sinks, which will be passed every 
--   chunk that is pushed to each sink.
--
--   * The worker is also passed the source index of the chunk that was pushed.
--
watch_o :: (Int -> Vector r a -> IO ())
        -> Sinks r a    -> IO (Sinks r a)
watch_o f k = G.watch_o (\(IIx i _) vec -> f i vec) k
{-# INLINE watch_o #-}


-- | Create a bundle of sinks of the given arity that pass incoming chunks
--   to a worker function. 
--
--   * This is like `watch_o`, except that the incoming chunks are discarded
--     after they are passed to the worker function
--
trigger_o :: Int -> (Int -> Vector r a -> IO ()) 
          -> IO (Sinks r a)
trigger_o arity f 
        = G.trigger_o arity (\(IIx i _) vec -> f i vec)
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
discard_o :: Int -> IO (Sinks r a)
discard_o = G.discard_o
{-# INLINE discard_o #-}


-- | Create a bundle of sinks of the given arity that drop all data on the
--   floor. 
--
--   * As opposed to `discard_o` the sinks are non-strict in the chunks.
--   * Haskell debugging thunks attached to the chunks will *not* be 
--     demanded.
--
ignore_o :: Int -> IO (Sinks r a)
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
head_i  :: A.Window r DIM1 a
        => Int -> Int -> Sources r a -> IO (Maybe ([a], Sources r a))
head_i ix len s
 | ix >= G.sourceArity s = return Nothing
 | otherwise             
 = liftM Just $ C.head_i len s (IIx ix (G.sourceArity s))
{-# INLINE head_i #-}


-- Grouping -------------------------------------------------------------------
-- | Scan through a some sources to find runs of matching elements, 
--   and count the lengths of those runs.
--
-- @  
-- > toList1 0 =<< groups_i U =<< fromList U 1 "waabbbblle"
-- Just [(\'w\',1),(\'a\',2),(\'b\',4),(\'l\',2),(\'e\',1)]
-- @
--
groups_i
        :: ( A.Bulk   rElt DIM1 a
           , A.Target rGrp a    tGrp
           , A.Target rLen Int  tLen
           , Eq a)
        => rGrp                 -- ^ Representation of result chunks.
        -> rLen
        -> Sources rElt a         -- ^ Input elements.
        -> IO (Sources (T2 rGrp rLen) (a, Int)) -- ^ Starting element and length of groups.
groups_i rGrp rLen s
        = groupsBy_i rGrp rLen (==) s
{-# INLINE groups_i #-}


-- | Like `groupsBy`, but take a function to determine whether two consecutive
--   values should be in the same group.
groupsBy_i
        :: ( A.Bulk   rElt DIM1 a
           , A.Target rGrp a   tGrp
           , A.Target rLen Int tLen)
        => rGrp                 -- ^ Representation of result chunks.
        -> rLen
        -> (a -> a -> Bool)     -- ^ Fn to check if consecutive elements
                                --   are in the same group.
        -> Sources rElt a       -- ^ Input elements.
        -> IO (Sources (T2 rGrp rLen) (a, Int)) -- ^ Starting element and length of groups.
groupsBy_i _ _ f s
        = C.groupsBy_i f s
{-# INLINE groupsBy_i #-}


-- Folding --------------------------------------------------------------------
-- | Given streams of lengths and values, perform a segmented fold where
--   fold segments of values of the corresponding lengths are folded 
--   together.
--
-- @
-- > sLens <- fromList U 1 [1, 2, 4, 0, 1, 5 :: Int]
-- > sVals <- fromList U 1 [10, 20, 30, 40, 50, 60, 70, 80, 90 :: Int]
-- > toList1 0 =<< folds_i U (+) 0 sLens sVals
-- Just [10,50,220,0,80]
-- @
--
--   If not enough input elements are available to fold a complete segment
--   then no output is produced for that segment. However, trailing zero
--   length segments still produce the initial value for the fold.
--
-- @
-- > sLens <- fromList U 1 [1, 2, 0, 0, 0 :: Int]
-- > sVals <- fromList U 1 [10, 20, 30 :: Int]
-- > toList1 0 =<< folds_i U (*) 1 sLens sVals
-- Just [10,600,1,1,1]
-- @
--
folds_i :: (FoldsWorthy rSeg rElt rGrp rRes tSeg tElt tGrp tRes n a b)
        => rGrp                   -- ^ Groups  chunk representation.
        -> rRes                   -- ^ Results chunk representation.
        -> (a -> b -> b)          -- ^ Worker function.
        -> b                      -- ^ Initial state when folding each segment.
        -> Sources rSeg (n, Int)  -- ^ Segment lengths.
        -> Sources rElt a         -- ^ Input elements to fold.
        -> IO (Sources (T2 rGrp rRes) (n, b)) -- ^ Result elements.

folds_i _ _ f z sLen sVal
        = C.folds_i f z sLen sVal
{-# INLINE folds_i #-}

-- | Type class dictionaries needed to perform a segmented fold.
--  
--   The chunks of all streams must have material representations,
--   rather than being delayed.
--
type FoldsWorthy rSeg rElt rGrp rRes tSeg tElt tGrp tRes n a b
 =      ( A.Material rSeg tSeg DIM1 (n, Int)
        , A.Material rElt tElt DIM1 a
        , A.Material rGrp tGrp DIM1 n
        , A.Material rRes tRes DIM1 b)


-- | Combination of `groupsBy_i` and `folds_i`. We determine the the segment
--   lengths while performing the folds.
-- 
--   Note that a SQL-like groupby aggregations can be performed using this 
--   function, provided the data is pre-sorted on the group key. For example,
--   we can take the average of some groups of values:
--
-- @
-- > sKeys   <-  fromList U 1 "waaaabllle"
-- > sVals   <-  fromList U 1 [10, 20, 30, 40, 50, 60, 70, 80, 90, 100 :: Int]
-- 
-- > sResult \<-  map_i U (\\(acc, n) -\> acc / n)
--           =\<\< foldGroupsBy_i U (==) (\\x (acc, n) -> (acc + x, n + 1)) (0, 0)
--
-- > toLists1 0 sResult
-- Just [10.0,35.0,60.0,80.0,100.0]
-- @
--
foldGroupsBy_i
        :: ( FoldsWorthy rSeg rElt rGrp rRes tSeg tElt tGrp tRes n a b
           , Bulk rSeg DIM1 n)
        => rGrp                 -- ^ Groups chunk representation.
        -> rRes                 -- ^ Result chunk representation.
        -> (n -> n -> Bool)     -- ^ Fn to check if consecutive elements
                                --   are in the same group.
        -> (a -> b -> b)        -- ^ Worker function for the fold.
        -> b                    -- ^ Initial when folding each segment.
        -> Sources rSeg n       -- ^ Names that determine groups.
        -> Sources rElt a       -- ^ Values to fold.
        -> IO (Sources (T2 rGrp rRes) (n, b))

foldGroupsBy_i rGrp rRes pGroup f z sNames sVals
 = do   segLens <- groupsBy_i rGrp U.U pGroup sNames
        folds_i rGrp rRes f z segLens sVals
{-# INLINE foldGroupsBy_i #-}

