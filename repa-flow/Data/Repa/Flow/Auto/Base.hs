
module Data.Repa.Flow.Auto.Base
        ( Sources
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
        , toArray1,             toArrays1)
where
import Data.Repa.Array.Auto
        hiding (fromList, fromLists)

import Data.Repa.Fusion.Unpack
import Data.Repa.Array.Material.Auto                    (A(..), Name(..))
import qualified Data.Repa.Flow.Chunked                 as C
import qualified Data.Repa.Flow.Generic                 as G
import qualified Data.Repa.Array.Meta.Window            as A
import qualified Data.Repa.Array.Auto                   as A
#include "repa-flow.h"


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
{-# INLINE_FLOW sourcesArity #-}


-- | Yield the number of streams in the bundle.
sinksArity :: Sinks a -> Int
sinksArity = G.sinksArity
{-# INLINE_FLOW sinksArity #-}


-- Conversion -----------------------------------------------------------------
-- | Given an arity and a list of elements,
--   yield sources that each produce all the elements. 
--
--   * All elements are stuffed into a single chunk,
--     and each stream is given the same chunk.
--
fromList :: Build a t
         => Int -> [a] -> IO (Sources a)
fromList xs = C.fromList A xs
{-# INLINE_FLOW fromList #-}


-- | Like `fromList` but take a list of lists.
--   Each each of the inner lists is packed into a single chunk.
fromLists :: Build a t
          => Int -> [[a]] -> IO (Sources a)
fromLists xss = C.fromLists A xss
{-# INLINE_FLOW fromLists #-}


-- | Drain a single source from a bundle into a list of elements.
--
--   * If the index does not specify a valid stream then the result will
--     be empty.
--  
toList1   :: Build a t
          => Int -> Sources a -> IO [a]
toList1 ix s  
 | ix < 0 || ix >= G.sourcesArity s 
 = return []

 | otherwise
 = C.toList1 ix s 
{-# INLINE_FLOW toList1 #-}


-- | Drain a single source from a bundle into a list of chunks.
--
--   * If the index does not specify a valid stream then the result will 
--     be empty.
--
toLists1  :: Build a t
          => Int -> Sources a -> IO [[a]]
toLists1 ix s
 | ix < 0 || ix >= G.sourcesArity s 
 = return []

 | otherwise
 = C.toLists1 ix s 
{-# INLINE_FLOW toLists1 #-}


-------------------------------------------------------------------------------
-- | Given an arity and an array of elements,
--   yield sources that each produce all the elements.
--
--   * All elements are stuffed into a single chunk,
--     and each stream is given the same chunk.
--
fromArray :: Build a t
          => Int -> Array a -> IO (Sources a)
fromArray n arr = G.fromList n [arr]
{-# INLINE_FLOW fromArray #-}


-- | Like `fromArray` but take an array of arrays.
--   Each of the inner arrays is packed into a single chunk.
fromArrays :: (Elem a, Build a t)
           => Int -> Array (Array a) -> IO (Sources a)
fromArrays n arrs = G.fromList n (A.toList arrs)
{-# INLINE_FLOW fromArrays #-}


-- | Drain a single source from a bundle into an array of elements.
--
--   * If the index does not specify a valid stream then the result will
--     be empty.
--
toArray1   :: (Elem a, Build a t, Unpack (Array a) att)
           => Int -> Sources a -> IO (Array a)
toArray1 ix ss
 | ix < 0 || ix >= G.sourcesArity ss
 = return $ A.fromList []

 | otherwise
 = do   chunks  <- G.toList1 ix ss
        return  $  A.concat $ A.fromList chunks
{-# INLINE_FLOW toArray1 #-}


-- | Drain a single source from a bundle into an array of elements.
--
--   * If the index does not specify a valid stream then the result will
--     be empty.
--
toArrays1  :: (Elem a, Build a t, Unpack (Array a) att)
           => Int -> Sources a -> IO (Array (Array a))
toArrays1 ix ss
 | ix < 0 || ix >= G.sourcesArity ss
 = return $ A.fromList []

 | otherwise
 = do   chunks  <- G.toList1 ix ss
        return  $ A.fromList chunks
{-# INLINE_FLOW toArrays1 #-}

