
module Data.Repa.Flow
        ( module Data.Repa.Flow.States

        -- * Flow types
        , Sources, Sinks
        , Flow

        -- * Representations
        -- | These are the representations that can be used for the 
        --   individual chunks in a flow.
        , A.Bulk, A.DIM1
        , U(..), B(..), F(..)

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

        -- ** Watching
        , watch_i,      watch_o
        , trigger_o

        -- ** Ignorance
        , discard_o
        , ignore_o

        -- ** Grouping
        , groupsBy_i)
where
import Data.Repa.Eval.Array                     as A
import Data.Repa.Array.Foreign                  as A
import Data.Repa.Flow.States
import Data.Repa.Array                          (DIM1, Vector)
import qualified Data.Repa.Array                as A
import qualified Data.Repa.Flow.Chunked         as C
import qualified Data.Repa.Flow.Generic         as G
import Control.Monad

import Data.Repa.Array                          (U(..), B(..))


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


-- Conversion -----------------------------------------------------------------
-- | Given an arity and a list of elements, yield sources that each produce all
--   the elements. 
--
--   * All elements are stuffed into a single chunk, and each stream is given
--     the same chunk.
--
fromList :: A.Target r a t
         => r -> Int -> [a] -> IO (Sources r a)
fromList _ xs = C.fromList_i xs
{-# INLINE fromList #-}


-- | Like `fromLists_i` but take a list of lists. Each each of the inner
--   lists is packed into a single chunk.
fromLists :: A.Target r a t
          => r -> Int -> [[a]] -> IO (Sources r a)
fromLists _ xss = C.fromLists_i xss
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
        => r2 -> (a -> b) -> Sinks r2 b   -> IO (Sinks   r1 a)
map_o _ f s = C.map_o f s
{-# INLINE map_o #-}


-- | Apply a function to all elements pulled from some sources,
--   a chunk at a time.
mapChunks_i  
        :: r2 -> (Vector r1 a -> Vector r2 b)
        -> Sources r1 a -> IO (Sources r2 b)
mapChunks_i _ f s 
        = G.smap_i (\_ c -> f c) s
{-# INLINE mapChunks_i #-}


-- | Apply a function to all elements pushed to some sinks,
--   a chunk at a time.
mapChunks_o  
        :: r2 -> (Vector r1 a -> Vector r2 b)
        -> Sinks r2 b -> IO (Sinks r1 a)
mapChunks_o _ f s 
        = G.smap_o (\_ c -> f c) s
{-# INLINE mapChunks_o #-}


-- | Like `mapChunks_i`, except that the worker function is also given
--   the source index.
smapChunks_i  
        :: r2 -> (Int -> Vector r1 a -> Vector r2 b)
        -> Sources r1 a -> IO (Sources r2 b)
smapChunks_i _ f s
        = G.smap_i (\(IIx i _) vec -> f i vec) s
{-# INLINE smapChunks_i #-}


-- | Like `mapChunks_o`, except that the worker function is also given
--   the sink index.
smapChunks_o  
        :: r2 -> (Int -> Vector r1 a -> Vector r2 b)
        -> Sinks r2 b -> IO (Sinks r1 a)
smapChunks_o _ f k
        = G.smap_o (\(IIx i _) vec -> f i vec) k
{-# INLINE smapChunks_o #-}


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


-- Grouping -------------------------------------------------------------------
-- | Scan through a some sources to find runs of matching elements, 
--   and count the lengths of those runs.
--
-- @  
--   > toList1 0 =<< groupsBy_i B (==) =<< fromList B 1 "waabbbblle"
--   Just [('w',1),('a',2),('b',4),('l',2),('e',1)]
-- @
-- 
groupsBy_i
        :: (A.Bulk r1 DIM1 a, A.Target r2 (a, Int) t2)
        => r2                       -- ^ Representation of result chunks.
        -> (a -> a -> Bool)         -- ^ Fn to check if consecutive elements
                                    --   are in the same group.
        -> Sources r1 a             -- ^ Input elements.
        -> IO (Sources r2 (a, Int)) -- ^ Starting element and length of groups.
groupsBy_i _ f s
        = C.groupsBy_i f s
{-# INLINE groupsBy_i #-}




