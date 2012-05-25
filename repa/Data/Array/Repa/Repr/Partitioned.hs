

module Data.Array.Repa.Repr.Partitioned
        ( P, Array (..)
        , Range(..)
        , inRange)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.Delayed


-- | Partitioned arrays.
--   The last partition takes priority
--
--   These are produced by Repa's support functions and allow arrays to be defined
--   using a different element function for each partition.
--
--   The basic idea is described in ``Efficient Parallel Stencil Convolution'',
--   Ben Lippmeier and Gabriele Keller, Haskell 2011 -- though the underlying
--   array representation has changed since this paper was published.
--
data P r1 r2

data Range sh
        = Range !sh !sh                      -- indices defining the range
                (sh -> Bool)                 -- predicate to check whether were in range

-- | Check whether an index is within the given range.
inRange :: Range sh -> sh -> Bool
inRange (Range _ _ p) ix
        = p ix
{-# INLINE inRange #-}


-- Repr -----------------------------------------------------------------------
-- | Read elements from a partitioned array.
instance (Source r1 e, Source r2 e) => Source (P r1 r2) e where
 data Array (P r1 r2) sh e
        = APart !sh                          -- size of the whole array
                !(Range sh) !(Array r1 sh e) -- if in range use this array
                !(Array r2 sh e)             -- otherwise use this array


 index (APart _ range arr1 arr2) ix
   | inRange range ix   = index arr1 ix
   | otherwise          = index arr2 ix
 {-# INLINE index #-}

 linearIndex arr@(APart sh _ _ _) ix
        = index arr $ fromIndex sh ix
 {-# INLINE linearIndex #-}

 extent (APart sh _ _ _) 
        = sh
 {-# INLINE extent #-}

 deepSeqArray (APart sh range arr1 arr2) y
  = sh `deepSeq` range `deepSeqRange` arr1 `deepSeqArray` arr2 `deepSeqArray` y
 {-# INLINE deepSeqArray #-}


deepSeqRange :: Shape sh => Range sh -> b -> b
deepSeqRange (Range ix sz f) y
        = ix `deepSeq` sz `deepSeq` f `seq` y
{-# INLINE deepSeqRange #-}


-- Load -----------------------------------------------------------------------
instance (LoadRange r1 sh e, Load r2 sh e)
        => Load (P r1 r2) sh e where
 loadP (APart _ (Range ix sz _) arr1 arr2) marr
  = do  loadRangeP arr1 marr ix sz
        loadP arr2 marr
 {-# INLINE loadP #-}

 loadS (APart _ (Range ix sz _) arr1 arr2) marr
  = do  loadRangeS arr1 marr ix sz
        loadS arr2 marr
 {-# INLINE loadS #-}



