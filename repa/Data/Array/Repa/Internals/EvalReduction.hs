{-# LANGUAGE BangPatterns #-}
module Data.Array.Repa.Internals.EvalReduction 
        ( foldS,    foldP
        , foldAllS, foldAllP)
where
import Data.Array.Repa.Internals.Elt
import Data.Array.Repa.Internals.Gang
import qualified Data.Vector.Unboxed            as V
import qualified Data.Vector.Unboxed.Mutable    as M
import GHC.Base                                 ( quotInt, divInt )


-- | Sequential reduction of a multidimensional array along the innermost dimension.
foldS :: Elt a
      => M.IOVector a           -- ^ vector to write elements into
      -> (Int -> a)             -- ^ function to get an element from the given index
      -> (a -> a -> a)          -- ^ binary associative combination function
      -> a                      -- ^ starting value (typically an identity)
      -> Int                    -- ^ inner dimension (length to fold over)
      -> IO ()
{-# INLINE foldS #-}
foldS vec !f !c !r !n = iter 0 0
  where
    !end = M.length vec

    {-# INLINE iter #-}
    iter !sh !sz | sh >= end = return ()
                 | otherwise =
                     let !next = sz + n
                     in  M.unsafeWrite vec sh (reduce f c r sz next) >> iter (sh+1) next


-- | Parallel reduction of a multidimensional array along the innermost dimension.
--   Each output value is computed by a single thread, with the output values
--   distributed evenly amongst the available threads.
foldP :: Elt a
      => M.IOVector a           -- ^ vector to write elements into
      -> (Int -> a)             -- ^ function to get an element from the given index
      -> (a -> a -> a)          -- ^ binary associative combination operator 
      -> a                      -- ^ starting value. Must be neutral with respect
                                -- ^ to the operator. eg @0 + a = a@.
      -> Int                    -- ^ inner dimension (length to fold over)
      -> IO ()
{-# INLINE foldP #-}
foldP vec !f !c !r !n
  = gangIO theGang
  $ \tid -> fill (split tid) (split (tid+1))
  where
    !threads  = gangSize theGang
    !len      = M.length vec
    !step     = (len + threads - 1) `quotInt` threads

    {-# INLINE split #-}
    split !ix = len `min` (ix * step)

    {-# INLINE fill #-}
    fill !start !end = iter start (start * n)
      where
        {-# INLINE iter #-}
        iter !sh !sz | sh >= end = return ()
                     | otherwise =
                         let !next = sz + n
                         in  M.unsafeWrite vec sh (reduce f c r sz next) >> iter (sh+1) next


-- | Sequential reduction of all the elements in an array.
foldAllS :: Elt a
         => (Int -> a)          -- ^ function to get an element from the given index
         -> (a -> a -> a)       -- ^ binary associative combining function
         -> a                   -- ^ starting value
         -> Int                 -- ^ number of elements
         -> IO a
{-# INLINE foldAllS #-}
foldAllS !f !c !r !len = return $! reduce f c r 0 len


-- | Parallel tree reduction of an array to a single value. Each thread takes an
--   equally sized chunk of the data and computes a partial sum. The main thread
--   then reduces the array of partial sums to the final result.
--
--   We don't require that the initial value be a neutral element, so each thread
--   computes a fold1 on its chunk of the data, and the seed element is only
--   applied in the final reduction step.
--
foldAllP :: Elt a
         => (Int -> a)          -- ^ function to get an element from the given index
         -> (a -> a -> a)       -- ^ binary associative combining function
         -> a                   -- ^ starting value
         -> Int                 -- ^ number of elements
         -> IO a
{-# INLINE foldAllP #-}
foldAllP !f !c !r !len
  | len == 0    = return r
  | otherwise   = do
      mvec <- M.unsafeNew chunks
      gangIO theGang $ \tid -> fill mvec tid (split tid) (split (tid+1))
      vec  <- V.unsafeFreeze mvec
      return $! V.foldl' c r vec
  where
    !threads    = gangSize theGang
    !step       = (len + threads - 1) `quotInt` threads
    chunks      = ((len + step - 1) `divInt` step) `min` threads

    {-# INLINE split #-}
    split !ix   = len `min` (ix * step)

    {-# INLINE fill #-}
    fill !mvec !tid !start !end
      | start >= end = return ()
      | otherwise    = M.unsafeWrite mvec tid (reduce f c (f start) (start+1) end)


-- | Sequentially reduce values between the given indices
{-# INLINE reduce #-}
reduce :: (Int -> a) -> (a -> a -> a) -> a -> Int -> Int -> a
reduce !f !c !r !start !end = iter start r
  where
    {-# INLINE iter #-}
    iter !i !z | i >= end  = z
               | otherwise = iter (i+1) (f i `c` z)

