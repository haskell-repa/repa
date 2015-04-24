
module Data.Repa.Flow.Auto.Base
        ( Sources
        , Sinks
        , Flow
        , sourcesArity
        , sinksArity

        -- * Conversion
        , fromList,             fromLists
        , toList1,              toLists1)
where
import Data.Repa.Array.Auto
        hiding (fromList, fromLists)

import Data.Repa.Array.Material.Auto                    (A(..), Name(..))
import qualified Data.Repa.Flow.Chunked                 as C
import qualified Data.Repa.Flow.Generic                 as G
import qualified Data.Repa.Array.Meta.Window            as A


-- | A bundle of stream sources, where the elements of the stream
--   are chunked into arrays.
type Sources a  = C.Sources Int IO A a


-- | A bundle of stream sinks,   where the elements of the stream
--   are chunked into arrays.
--
type Sinks a    = C.Sinks Int IO A a


-- | Shorthand for common type classes.
type Flow a     = (C.Flow  Int IO A a, A.Windowable A a)


-- | Yield the number of streams in the bundle.
sourcesArity :: Sources a -> Int
sourcesArity = G.sourcesArity


-- | Yield the number of streams in the bundle.
sinksArity :: Sinks a -> Int
sinksArity = G.sinksArity


-- Conversion -----------------------------------------------------------------
-- | Given an arity and a list of elements, yield sources that each produce all
--   the elements. 
--
--   * All elements are stuffed into a single chunk, and each stream is given
--     the same chunk.
--
fromList :: Build a t
         => Int -> [a] -> IO (Sources a)
fromList xs = C.fromList A xs
{-# INLINE fromList #-}


-- | Like `fromLists_i` but take a list of lists. Each each of the inner
--   lists is packed into a single chunk.
fromLists :: Build a t
          => Int -> [[a]] -> IO (Sources a)
fromLists xss = C.fromLists A xss
{-# INLINE fromLists #-}


-- | Drain a single source from a bundle into a list of elements.
toList1   :: Build a t
          => Int -> Sources a -> IO [a]
toList1 ix s  
 | ix >= G.sourcesArity s = return []
 | otherwise              = C.toList1 ix s 
{-# INLINE toList1 #-}


-- | Drain a single source from a bundle into a list of chunks.
toLists1  :: Build a t
          => Int -> Sources a -> IO [[a]]
toLists1 ix s
 | ix >= G.sourcesArity s = return []
 | otherwise              = C.toLists1 ix s 
{-# INLINE toLists1 #-}

