
module Data.Repa.Vector.Unboxed
        ( -- * Stream operators
          -- ** Compacting
          compact
        , compactIn

          -- ** Dicing
        , findSegments
        , findSegmentsFrom
        , diceSep

          -- ** Extracting
        , extract

          -- ** Inserting
        , insert

          -- ** Merging
        , merge
        , mergeMaybe

          -- ** Padding
        , padForward

          -- ** Ratcheting
        , ratchet

          -- * Chain operators
        , unchainToVector
        , unchainToMVector

          -- ** Folding
        , folds, C.Folds(..)

          -- ** Scanning
        , scanMaybe
        , groupsBy

          -- ** Conversion
        , chainOfVector
        )
where
import Data.Vector.Unboxed                              (Unbox, Vector)
import Data.Vector.Unboxed.Mutable                      (MVector)
import Data.Repa.Chain                                  (Chain)
import Data.Repa.Scalar.Option
import qualified Data.Repa.Vector.Generic               as G
import qualified Data.Repa.Chain                        as C
import qualified Data.Vector.Unboxed                    as U
import Control.Monad.Primitive
#include "repa-stream.h"


-------------------------------------------------------------------------------
-- | Produce a chain from a generic vector.
chainOfVector 
        :: (Monad m, Unbox a)
        => Vector a -> Chain m Int a
chainOfVector = G.chainOfVector
{-# INLINE chainOfVector #-}


-- | Compute a chain into a vector.
unchainToVector
        :: (PrimMonad m, Unbox a)
        => C.Chain m s a  -> m (Vector a, s)
unchainToVector = G.unchainToVector
{-# INLINE unchainToVector #-}


-- | Compute a chain into a mutable vector.
unchainToMVector
        :: (PrimMonad m, Unbox a)
        => C.Chain m s a
        -> m (MVector (PrimState m) a, s)
unchainToMVector = G.unchainToMVector
{-# INLINE unchainToMVector #-}


-------------------------------------------------------------------------------
-- | Interleaved `enumFromTo`. 
--
--   Given a vector of starting values, and a vector of stopping values, 
--   produce an stream of elements where we increase each of the starting
--   values to the stopping values in a round-robin order. Also produce a
--   vector of result segment lengths.
--
-- @
--  unsafeRatchetS [10,20,30,40] [15,26,33,47]
--  =  [10,20,30,40       -- 4
--     ,11,21,31,41       -- 4
--     ,12,22,32,42       -- 4
--     ,13,23   ,43       -- 3
--     ,14,24   ,44       -- 3
--        ,25   ,45       -- 2
--              ,46]      -- 1
--
--         ^^^^             ^^^
--       Elements         Lengths
-- @
--
ratchet :: U.Vector (Int, Int)          -- ^ Starting and ending values.
        -> (U.Vector Int, U.Vector Int) -- ^ Elements and Lengths vectors.
ratchet = G.ratchet 
{-# INLINE ratchet #-}


-------------------------------------------------------------------------------
-- | Extract segments from some source array and concatenate them.
-- 
-- @
--    let arr = [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
--    in  extractS (index arr) [(0, 1), (3, 3), (2, 6)]
--    
--     => [10, 13, 14, 15, 12, 13, 14, 15, 16, 17]
-- @
--
extract :: Unbox a 
        => (Int -> a)           -- ^ Function to get elements from the source.
        -> U.Vector (Int, Int)  -- ^ Segment starts and lengths.
        -> U.Vector a           -- ^ Result elements.

extract = G.extract
{-# INLINE extract #-}


-------------------------------------------------------------------------------
-- | Insert elements produced by the given function into a vector.
insert   :: Unbox a
        => (Int -> Maybe a)     -- ^ Produce a new element for this index.
        -> U.Vector a           -- ^ Source vector.
        -> U.Vector a

insert = G.insert
{-# INLINE insert #-}


-------------------------------------------------------------------------------
-- | Merge two pre-sorted key-value streams.
merge   :: (Ord k, Unbox k, Unbox a, Unbox b, Unbox c)
        => (k -> a -> b -> c)   -- ^ Combine two values with the same key.
        -> (k -> a -> c)        -- ^ Handle a left value without a right value.
        -> (k -> b -> c)        -- ^ Handle a right value without a left value.
        -> U.Vector (k, a)      -- ^ Vector of keys and left values.
        -> U.Vector (k, b)      -- ^ Vector of keys and right values.
        -> U.Vector (k, c)      -- ^ Vector of keys and results.

merge = G.merge
{-# INLINE merge #-}


-- | Like `merge`, but only produce the elements where the worker functions
--   return `Just`.
mergeMaybe 
        :: (Ord k, Unbox k, Unbox a, Unbox b, Unbox c)
        => (k -> a -> b -> Maybe c) -- ^ Combine two values with the same key.
        -> (k -> a -> Maybe c)      -- ^ Handle a left value without a right value.
        -> (k -> b -> Maybe c)      -- ^ Handle a right value without a left value.
        -> U.Vector (k, a)          -- ^ Vector of keys and left values.
        -> U.Vector (k, b)          -- ^ Vector of keys and right values.
        -> U.Vector (k, c)          -- ^ Vector of keys and results.

mergeMaybe = G.mergeMaybe
{-# INLINE mergeMaybe #-}


-------------------------------------------------------------------------------
-- | Perform a left-to-right scan through an input vector, maintaining a state
--   value between each element. For each element of input we may or may not
--   produce an element of output.
scanMaybe 
        :: (Unbox a, Unbox b)
        => (s -> a -> (s, Maybe b))     -- ^ Worker function.
        ->  s                           -- ^ Initial state for scan.
        ->  U.Vector a                  -- ^ Input elements.
        -> (U.Vector b, s)              -- ^ Output elements.

scanMaybe = G.scanMaybe
{-# INLINE scanMaybe #-}


-------------------------------------------------------------------------------
-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
-- 
-- @
--  groupsBy (==) (Just ('a', 4)) 
--                [\'a\', \'a\', \'a\', \'b\', \'b\', \'c\', \'d\', \'d\'] 
--   => ([('a', 7), ('b', 2), ('c', 1)], Just (\'d\', 2))
-- @
--
groupsBy
        :: Unbox a
        => (a -> a -> Bool)             -- ^ Comparison function.
        -> Maybe (a, Int)               -- ^ Starting element and count.
        ->  U.Vector a                  -- ^ Input elements.
        -> (U.Vector (a, Int), Maybe (a, Int))

groupsBy = G.groupsBy
{-# INLINE groupsBy #-}


-------------------------------------------------------------------------------
-- | Given predicates that detect the beginning and end of some interesting
--   segment of information, scan through a vector looking for when these
--   segments begin and end. Return vectors of the segment starting positions
--   and lengths.
--
--   * As each segment must end on a element where the ending predicate returns
--     True, the miniumum segment length returned is 1.
--
findSegments 
        :: U.Unbox a 
        => (a -> Bool)          -- ^ Predicate to check for start of segment.
        -> (a -> Bool)          -- ^ Predicate to check for end of segment.
        ->  U.Vector a          -- ^ Input vector.
        -> (U.Vector Int, U.Vector Int)

findSegments = G.findSegments
{-# INLINE findSegments #-}


-------------------------------------------------------------------------------
-- | Given predicates that detect the beginning and end of some interesting
--   segment of information, scan through a vector looking for when these
--   segments begin and end. Return vectors of the segment starting positions
--   and lengths.
findSegmentsFrom
        :: (a -> Bool)          -- ^ Predicate to check for start of segment.
        -> (a -> Bool)          -- ^ Predicate to check for end of segment.
        -> Int                  -- ^ Input length.
        -> (Int -> a)           -- ^ Get an element from the input.
        -> (U.Vector Int, U.Vector Int)

findSegmentsFrom = G.findSegmentsFrom
{-# INLINE findSegmentsFrom #-}


-------------------------------------------------------------------------------
-- | Dice a vector stream into rows and columns.
--
diceSep :: Unbox a
        => (a -> Bool)  -- ^ Detect the end of a column.
        -> (a -> Bool)  -- ^ Detect the end of a row.
        -> U.Vector a
        -> (U.Vector (Int, Int), U.Vector (Int, Int))
                        -- ^ Segment starts   and lengths

diceSep = G.diceSep
{-# INLINE diceSep #-}


-------------------------------------------------------------------------------
-- | Combination of `fold` and `filter`. 
--   
--   We walk over the stream front to back, maintaining an accumulator.
--   At each point we can chose to emit an element (or not)
--
compact
        :: (Unbox a, Unbox b)
        => (s -> a -> (s, Maybe b))     -- ^ Worker function
        -> s                            -- ^ Starting state
        -> Vector a                     -- ^ Input vector
        -> Vector b

compact = G.compact
{-# INLINE compact #-}


-- | Like `compact` but use the first value of the stream as the 
--   initial state, and add the final state to the end of the output.
compactIn
        :: Unbox a
        => (a -> a -> (a, Maybe a))     -- ^ Worker function.
        -> Vector a                     -- ^ Input elements.
        -> Vector a

compactIn = G.compactIn
{-# INLINE compactIn #-}


-------------------------------------------------------------------------------
-- | Segmented fold over vectors of segment lengths and input values.
--
--   The total lengths of all segments need not match the length of the
--   input elements vector. The returned `C.Folds` state can be inspected
--   to determine whether all segments were completely folded, or the 
--   vector of segment lengths or elements was too short relative to the
--   other. In the resulting state, `C.foldLensState` is the index into
--   the lengths vector *after* the last one that was consumed. If this
--   equals the length of the lengths vector then all segment lengths were
--   consumed. Similarly for the elements vector.
--
folds   :: (Unbox n, Unbox a, Unbox b)
        => (a -> b -> b)        -- ^ Worker function to fold each segment.
        -> b                    -- ^ Initial state when folding segments.
        -> Option3 n Int b      -- ^ Length and initial state for first segment.
        -> U.Vector (n, Int)    -- ^ Segment names and lengths.
        -> U.Vector a           -- ^ Elements.
        -> (U.Vector (n, b), C.Folds Int Int n a b)

folds = G.folds
{-# INLINE folds #-}


-------------------------------------------------------------------------------
-- | Given a stream of keys and values, and a successor function for keys, 
--   if the stream is has keys missing in the sequence then insert 
--   the missing key, copying forward the the previous value.
padForward  
        :: (Unbox k, Unbox v, Ord k)
        => (k -> k)             -- ^ Successor function.
        -> U.Vector (k, v)      -- ^ Input keys and values.
        -> U.Vector (k, v)

padForward = G.padForward
{-# INLINE padForward #-}

