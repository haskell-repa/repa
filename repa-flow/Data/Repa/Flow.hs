
module Data.Repa.Flow
        ( module Data.Repa.Flow.States

        -- * Flow types
        , Sources, Sinks
        , Flow

        -- * Representations
        -- | These are the representations that can be used for the 
        --   individual chunks in a flow.
        , U(..), B(..), F(..)

        -- * Conversion
        , fromList,     fromLists
        , toList1,      toLists1

        -- * Finalizers
        , finalize_i,   finalize_o

        -- * Flow Operators
        -- ** Mapping
        , map_i,        map_o)
where
import Data.Repa.Eval.Array                     as A
import Data.Repa.Array.Foreign                  as A
import Data.Repa.Flow.States
import Data.Repa.Array                          (DIM1)
import qualified Data.Repa.Array                as A
import qualified Data.Repa.Flow.Chunked         as C
import qualified Data.Repa.Flow.Generic         as G
import Control.Monad

import Data.Repa.Array                          (U(..), B(..))
import Data.Repa.Array.Foreign                  (F(..))


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
          => Sources r a -> Int -> IO (Maybe [a])
toList1 s ix  
 | ix >= G.sourceArity s = return Nothing
 | otherwise             
 = liftM Just $ C.toList1_i s (IIx ix (G.sourceArity s))
{-# INLINE toList1 #-}


-- | Drain a single source from a bundle into a list of chunks.
toLists1  :: A.Bulk r DIM1 a
          => Sources r a -> Int -> IO (Maybe [[a]])
toLists1 s ix
 | ix >= G.sourceArity s = return Nothing
 | otherwise             
 = liftM Just $ C.toLists1_i s (IIx ix (G.sourceArity s))
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








