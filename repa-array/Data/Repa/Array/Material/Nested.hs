
module Data.Repa.Array.Material.Nested
        ( N(..)
        , U.Unbox
        , Array (..)

        -- * Conversion
        , fromLists
        , fromListss

        -- * Mapping
        , mapElems

        -- * Slicing
        , slices

        -- * Concatenation
        , concats

        -- * Splitting
        , segment, segmentOn
        , dice,    diceOn

        -- * Trimming
        , trims
        , trimEnds
        , trimStarts

        -- * Transpose
        , ragspose3)
where
import Data.Repa.Array.Delayed
import Data.Repa.Array.Window
import Data.Repa.Array.Index
import Data.Repa.Array.Material.Unboxed                 as A
import Data.Repa.Array.Internals.Bulk                   as A
import Data.Repa.Array.Internals.Target                 as A
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Repa.Vector.Unboxed               as U
import Prelude                                          as P
import Prelude  hiding (concat)
#include "repa-array.h"


-- | Nested array represented as a flat array of elements, and a segment
--   descriptor that describes how the elements are partitioned into
--   the sub-arrays. Using this representation for multidimentional arrays
--   is significantly more efficient than using a boxed array of arrays, 
--   as there is no need to allocate the sub-arrays individually in the heap.
--
--   With a nested type like:
--   @Array N (Array N (Array U Int))@, the concrete representation consists
--   of five flat unboxed vectors: two for each of the segment descriptors
--   associated with each level of nesting, and one unboxed vector to hold
--   all the integer elements.
--
--   UNSAFE: Indexing into raw material arrays is not bounds checked.
--   You may want to wrap this with a Checked layout as well.
--
data N  = Nested 
        { nestedLength  :: Int }


-------------------------------------------------------------------------------
instance Layout N where
 data Name  N           = N
 type Index N           = Int
 create N len           = Nested len
 extent (Nested len)    = len
 toIndex   _ ix         = ix
 fromIndex _ ix         = ix
 {-# INLINE extent    #-}
 {-# INLINE toIndex   #-}
 {-# INLINE fromIndex #-}


-------------------------------------------------------------------------------
-- | Nested arrays.
instance (Bulk l a, Windowable l a, Index l ~ Int)
      =>  Bulk N (Array l a) where

 data Array N (Array l a)
        = NArray !(U.Vector Int)        -- segment start positions.
                 !(U.Vector Int)        -- segment lengths.
                 !(Array l a)           -- data values

 layout (NArray starts _lengths _elems)
        = Nested (U.length starts)
 {-# INLINE_ARRAY layout #-}

 index  (NArray starts lengths elems) ix
        = window (starts  `U.unsafeIndex` ix)
                 (lengths `U.unsafeIndex` ix)
                 elems
 {-# INLINE_ARRAY index #-}


deriving instance Show (Array l a) => Show (Array N (Array l a))


-------------------------------------------------------------------------------
-- | Windowing Nested arrays.
instance (Bulk l a, Windowable l a, Index l ~ Int)
      => Windowable N (Array l a) where
 window start len (NArray starts lengths elems)
        = NArray  (U.unsafeSlice start len starts)
                  (U.unsafeSlice start len lengths)
                  elems
 {-# INLINE_ARRAY window #-}


-------------------------------------------------------------------------------
-- | O(size src) Convert some lists to a nested array.
fromLists 
        :: (Target l a, Index l ~ Int)
        => l -> [[a]] -> Array N (Array l a)
fromLists l xss
 = let  xs         = concat xss
        Just elems = fromList l xs
        lengths    = U.fromList    $ P.map P.length xss
        starts     = U.unsafeInit  $ U.scanl (+) 0 lengths
   in   NArray starts lengths elems
{-# INLINE_ARRAY fromLists #-}
        

-- | O(size src) Convert a triply nested list to a triply nested array.
fromListss 
        :: (Target l a, Index l ~ Int)
        => l -> [[[a]]] -> Array N (Array N (Array l a))
fromListss l xs
 = let  xs1        = concat xs
        xs2        = concat xs1
        Just elems = fromList l xs2
        
        lengths1   = U.fromList   $ P.map P.length xs
        starts1    = U.unsafeInit $ U.scanl (+) 0 lengths1

        lengths2   = U.fromList   $ P.map P.length xs1
        starts2    = U.unsafeInit $ U.scanl (+) 0 lengths2

   in   NArray    starts1 lengths1 
         $ NArray starts2 lengths2 
         $ elems
{-# INLINE_ARRAY fromListss #-}


-------------------------------------------------------------------------------
-- | Apply a function to all the elements of a doubly nested array,
--   preserving the nesting structure.
mapElems :: (Array l1 a -> Array l2 b)
         ->  Array N (Array l1 a)
         ->  Array N (Array l2 b)

mapElems f (NArray starts lengths elems)
 = NArray starts lengths (f elems)
{-# INLINE_ARRAY mapElems #-}


-------------------------------------------------------------------------------
-- | O(1). Produce a nested array by taking slices from some array of elements.
--   
--   This is a constant time operation, as the representation for nested 
--   vectors just wraps the starts, lengths and elements vectors.
--
slices  :: Array U Int                  -- ^ Segment starting positions.
        -> Array U Int                  -- ^ Segment lengths.
        -> Array l a                    -- ^ Array elements.
        -> Array N (Array l a)

slices (UArray starts) (UArray lens) !elems
 = NArray starts lens elems
{-# INLINE_ARRAY slices #-}


-------------------------------------------------------------------------------
-- | Segmented concatenation.
--   Concatenate triply nested vector, producing a doubly nested vector.
--
--   Example: @concats [[[10 11 12] [20 21]] [[30 31] [40] [50]]]
--                   = [ [10 11 12   20 21]   [30 31   40   50] ]@
--
--   * This version is more efficient than plain `concat` as the operation
--     is done entirely on the segment descriptors of the nested arrays.
--
concats :: Array N (Array N (Array l a)) 
        -> Array N (Array l a)

concats (NArray starts1 lengths1 (NArray starts2 lengths2 elems))
 = let
        starts2'        = U.extract (U.unsafeIndex starts2)
                        $ U.zip starts1 lengths1

        lengths2'       = U.extract (U.unsafeIndex lengths2)
                        $ U.zip starts1 lengths1

   in   NArray starts2' lengths2' elems
{-# INLINE_ARRAY concats #-}


-------------------------------------------------------------------------------
-- | O(len src). Given predicates which detect the start and end of a segment, 
--   split an vector into the indicated segments.
segment :: (U.Unbox a, Bulk l a, Index l ~ Int)
        => (a -> Bool)  -- ^ Detect the start of a segment.
        -> (a -> Bool)  -- ^ Detect the end of a segment.
        -> Array l a    -- ^ Vector to segment.
        -> Array N (Array l a)  

segment pStart pEnd !elems
 = let  len     = size (extent $ layout elems)
        (starts, lens)  
                = U.findSegments pStart pEnd 
                $ U.generate len (\ix -> index elems ix)

   in   NArray starts lens elems
{-# INLINE_ARRAY segment #-}


-- | O(len src). Given a terminating value, split an vector into segments.
--
--   The result segments do not include the terminator.
--  
--   @segmentOn ' ' "fresh fried fish" = ["fresh", "fried", "fish"]@
--
segmentOn 
        :: (Eq a, U.Unbox a, Bulk l a, Index l ~ Int)
        => (a -> Bool)  -- ^ Detect the end of a segment.
        -> Array l a    -- ^ Vector to segment.
        -> Array N (Array l a)

segmentOn !pEnd !arr
 = segment (const True) pEnd arr
{-# INLINE_ARRAY segmentOn #-}


-------------------------------------------------------------------------------
-- | O(len src). Like `segment`, but cut the source array twice.
dice    :: (U.Unbox a, Bulk l a, Windowable l a, Index l ~ Int)
        => (a -> Bool)  -- ^ Detect the start of an inner segment.
        -> (a -> Bool)  -- ^ Detect the end   of an inner segment.
        -> (a -> Bool)  -- ^ Detect the start of an outer segment.
        -> (a -> Bool)  -- ^ Detect the end   of an outer segment.
        -> Array l a    -- ^ Array to dice.
        -> Array N (Array N (Array l a))

dice pStart1 pEnd1 pStart2 pEnd2 !arr
 = let  lenArr           = size (extent $ layout arr)

        -- Do the inner segmentation.
        (starts1, lens1) = U.findSegments pStart1 pEnd1 
                         $ U.generate lenArr (index arr)

        -- To do the outer segmentation we want to check if the first
        -- and last characters in each of the inner segments match
        -- the predicates.
        pStart2' arr'    
         = pStart2 $ index arr' 0

        pEnd2'   arr'    
         = pEnd2   $ index arr' (size (extent $ layout arr') - 1)

        -- Do the outer segmentation.
        !lenArrInner     = U.length starts1
        !arrInner        = NArray starts1 lens1 arr
        (starts2, lens2) = U.findSegmentsFrom pStart2' pEnd2'
                                lenArrInner (index arrInner)

   in   NArray starts2 lens2 arrInner
{-# INLINE_ARRAY dice #-}


-- | O(len src). Given field and row terminating values, 
--   split an array into rows and fields.
--
-- @ 
--   toListss $ diceOn '\t' '\n' 
--            $ (vfromList "12\\t34\\t56\\n78\\t\\t9\\n\\t00" :: Vector B Char)
--    = [["12\\t","34\\t","56\\n"],["78\\t","\\t","9\\n"],["\\t", "00"]]
-- @
--
--   If you don't want the terminating values in the result then use 
--   `trimEnds` to trim them off.
-- 
diceOn  :: (U.Unbox a, Eq a, Bulk l a, Windowable l a, Index l ~ Int)
        => a            -- ^ Terminating element for inner segments.
        -> a            -- ^ Terminating element for outer segments.
        -> Array l a    -- ^ Vector to dice.
        -> Array N (Array N (Array l a))

diceOn !xEndWord !xEndLine !arr
        = dice  (const True) (\x -> x == xEndWord || x == xEndLine)
                (const True) (\x -> x == xEndLine)
                arr
{-# INLINE_ARRAY diceOn #-}


-------------------------------------------------------------------------------
-- | For each segment of a nested vector, trim elements off the start
--   and end of the segment that match the given predicate.
trims   :: (Bulk l a, Index l ~ Int)
        => (a -> Bool)
        -> Array N (Array l a)
        -> Array N (Array l a)

trims pTrim (NArray starts lengths elems)
 = let
        loop_trimEnds !start !len 
         | len == 0     = (start, len)
         | pTrim (elems `index` (start + len - 1))
                        = loop_trimEnds   start (len - 1)
         | otherwise    = loop_trimStarts start len
        {-# INLINE_INNER loop_trimEnds #-}

        loop_trimStarts !start !len 
         | len == 0     = (start, len)
         | pTrim (elems `index` (start + len - 1)) 
                        = loop_trimStarts (start + 1) (len - 1)
         | otherwise    = (start, len)
        {-# INLINE_INNER loop_trimStarts #-}

        (starts', lengths')
                = U.unzip $ U.zipWith loop_trimEnds starts lengths

   in   NArray starts' lengths' elems
{-# INLINE_ARRAY trims #-}


-- | For each segment of a nested vector, trim elements off the end of 
--   the segment that match the given predicate.
trimEnds :: (Bulk l a, Index l ~ Int)
         => (a -> Bool)
         -> Array N (Array l a)
         -> Array N (Array l a)

trimEnds pTrim (NArray starts lengths elems)
 = let
        loop_trimEnds !start !len 
         | len == 0     = 0
         | pTrim (elems `index` (start + len - 1)) 
                        = loop_trimEnds start (len - 1)
         | otherwise    = len
        {-# INLINE_INNER loop_trimEnds #-}

        lengths'        = U.zipWith loop_trimEnds starts lengths

   in   NArray starts lengths' elems
{-# INLINE_ARRAY trimEnds #-}


-- | For each segment of a nested vector, trim elements off the start of
--   the segment that match the given predicate.
trimStarts :: (Bulk l a, Index l ~ Int)
           => (a -> Bool)
           -> Array N (Array l a)
           -> Array N (Array l a)

trimStarts pTrim (NArray starts lengths elems)
 = let
        loop_trimStarts !start !len 
         | len == 0     = (start, len)
         | pTrim (elems `index` (start + len - 1))
                        = loop_trimStarts (start + 1) (len - 1)
         | otherwise    = (start, len)
        {-# INLINE_INNER loop_trimStarts #-}

        (starts', lengths')
                = U.unzip $ U.zipWith loop_trimStarts starts lengths

   in   NArray starts' lengths' elems
{-# INLINE_ARRAY trimStarts #-}


-------------------------------------------------------------------------------
-- | Ragged transpose of a triply nested array.
-- 
--   * This version is more efficient than plain `ragspose` as the operation
--     is done entirely on the segment descriptors of the nested arrays.
--
ragspose3 :: Array N (Array N (Array l a)) 
          -> Array N (Array N (Array l a))

ragspose3 (NArray starts1 lengths1 (NArray starts2 lengths2 elems))
 = let  
        startStops1       = U.zipWith (\s l -> (s, s + l)) starts1 lengths1
        (ixs', lengths1') = U.ratchet startStops1

        starts2'          = U.map (U.unsafeIndex starts2)  ixs'
        lengths2'         = U.map (U.unsafeIndex lengths2) ixs'

        starts1'          = U.unsafeInit $ U.scanl (+) 0 lengths1'

   in   NArray starts1' lengths1' (NArray starts2' lengths2' elems)
{-# NOINLINE ragspose3 #-}
--  NOINLINE Because the operation is entirely on the segment descriptor.
--           This function won't fuse with anything externally, 
--           and it does not need to be specialiased.

