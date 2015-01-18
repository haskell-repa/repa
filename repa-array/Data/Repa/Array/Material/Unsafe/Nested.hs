
module Data.Repa.Array.Material.Unsafe.Nested
        ( UN(..), U.Unbox
        , Array (..)

        -- * Conversion
        , fromLists
        , fromListss

        -- * Mapping
        , mapElems

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
import Data.Repa.Array.Shape
import Data.Repa.Array.Internals.Bulk                   as R
import Data.Repa.Array.Internals.Target                 as R
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Repa.Vector.Unboxed               as U
import Prelude                                          as P
import Prelude  hiding (concat)

---------------------------------------------------------------------------------------------------
-- | Nested array represented as a flat array of elements, and a segment
--   descriptor that describes how the elements are partitioned into
--   the sub-arrays. Using this representation for multidimentional arrays
--   is significantly more efficient than using a boxed array of arrays, 
--   as there is no need to allocate the sub-arrays individually in the heap.
--
--   With a nested type like:
--   @Array UN (Array UN (Array UU Int))@, the concrete representation consists
--   of five flat unboxed vectors: two for each of the segment descriptors
--   associated with each level of nesting, and one unboxed vector to hold
--   all the integer elements.
--
data UN = UN

-- | Unsafe Nested arrays.
instance Repr UN where
 repr = UN
 

-- | Unsafe Nested arrays.
instance ( Bulk   r DIM1 a 
         , Window r DIM1 a)
      => Bulk UN DIM1 (Vector r a) where

 data Array UN DIM1 (Vector r a)
        = UNArray 
                 !(U.Vector Int)        -- segment start positions.
                 !(U.Vector Int)        -- segment lengths.
                 !(Array r DIM1 a)      -- data values

 extent (UNArray starts _lengths _elems)
        = Z :. U.length starts
 {-# INLINE [1] extent #-}

 index  (UNArray starts lengths elems) (Z :. ix)
  = window (Z :. (starts  `U.unsafeIndex` ix)) 
           (Z :. (lengths `U.unsafeIndex` ix)) 
           elems
 {-# INLINE [1] index #-}


deriving instance Show (Vector r a) => Show (Vector UN (Vector r a))


-- Window -----------------------------------------------------------------------------------------
-- | Unsafe Nested windows.
instance (Bulk r DIM1 a, Window r DIM1 a)
      => Window UN DIM1 (Vector r a) where
 window (Z :. start) (Z :. len) (UNArray starts lengths elems)
        = UNArray (U.unsafeSlice start len starts)
                  (U.unsafeSlice start len lengths)
                  elems
 {-# INLINE window #-}


-- Conversion -------------------------------------------------------------------------------------
-- | O(size src) Convert some lists to a nested array.
fromLists 
        :: Target r a t
        => [[a]] -> Vector UN (Vector r a)
fromLists xss
 = let  xs         = concat xss
        Just elems = fromList_     (Z :. P.length xs) xs
        lengths    = U.fromList    $ P.map P.length xss
        starts     = U.unsafeInit  $ U.scanl (+) 0 lengths
   in   UNArray starts lengths elems
{-# INLINE [1] fromLists #-}
        

-- | O(size src) Convert a triply nested list to a triply nested array.
fromListss 
        :: Target r a t
        => [[[a]]] -> Vector UN (Vector UN (Vector r a))
fromListss xs
 = let  xs1        = concat xs
        xs2        = concat xs1
        Just elems = fromList_ (Z :. P.length xs2) xs2
        
        lengths1   = U.fromList   $ P.map P.length xs
        starts1    = U.unsafeInit $ U.scanl (+) 0 lengths1

        lengths2   = U.fromList   $ P.map P.length xs1
        starts2    = U.unsafeInit $ U.scanl (+) 0 lengths2

   in   UNArray    starts1 lengths1 
         $ UNArray starts2 lengths2 
         $ elems
{-# INLINE [1] fromListss #-}


---------------------------------------------------------------------------------------------------
-- | Apply a function to all the elements of a doubly nested array,
--   preserving the nesting structure.
mapElems :: (Vector r1 a -> Vector r2 b)
         ->  Vector UN (Vector r1 a)
         ->  Vector UN (Vector r2 b)

mapElems f (UNArray starts lengths elems)
 = UNArray starts lengths (f elems)
{-# INLINE [1] mapElems #-}


---------------------------------------------------------------------------------------------------
-- | Segmented concatenation.
--   Concatenate triply nested vector, producing a doubly nested vector.
--
--   Example: @concats [[[10 11 12] [20 21]] [[30 31] [40] [50]]]
--                   = [ [10 11 12   20 21]   [30 31   40   50] ]@
--
--   * This version is more efficient than plain `concat` as the operation
--     is done entirely on the segment descriptors of the nested arrays.
--
concats :: Vector UN (Vector UN (Vector r a)) 
        -> Vector UN (Vector r a)

concats (UNArray starts1 lengths1 (UNArray starts2 lengths2 elems))
 = let
        starts2'        = U.extract (U.unsafeIndex starts2)
                        $ U.zip starts1 lengths1

        lengths2'       = U.extract (U.unsafeIndex lengths2)
                        $ U.zip starts1 lengths1

   in   UNArray starts2' lengths2' elems
{-# INLINE [1] concats #-}


---------------------------------------------------------------------------------------------------
-- | O(len src). Given predicates which detect the start and end of a segment, 
--   split an vector into the indicated segments.
segment :: (U.Unbox a, Bulk r DIM1 a)
        => (a -> Bool)  -- ^ Detect the start of a segment.
        -> (a -> Bool)  -- ^ Detect the end of a segment.
        -> Vector r a   -- ^ Vector to segment.
        -> Vector UN (Vector r a)  

segment pStart pEnd !elems
 = let  len     = size (extent elems)
        (starts, lens)  
                = U.findSegments pStart pEnd 
                $ U.generate len (\ix -> index elems (Z :. ix))

   in   UNArray starts lens elems
{-# INLINE [1] segment #-}


-- | O(len src). Given a terminating value, split an vector into segments.
--
--   The result segments do not include the terminator.
--  
--   @segmentOn ' ' "fresh fried fish" = ["fresh", "fried", "fish"]@
--
segmentOn 
        :: (Eq a, U.Unbox a, Bulk r DIM1 a)
        => (a -> Bool)  -- ^ Detect the end of a segment.
        -> Vector r a   -- ^ Vector to segment.
        -> Vector UN (Vector r a)

segmentOn !pEnd !arr
 = segment (const True) pEnd arr
{-# INLINE [1] segmentOn #-}


---------------------------------------------------------------------------------------------------
-- | O(len src). Like `segment`, but cut the source array twice.
dice    :: (U.Unbox a, Bulk r DIM1 a, Window r DIM1 a)
        => (a -> Bool)  -- ^ Detect the start of an inner segment.
        -> (a -> Bool)  -- ^ Detect the end   of an inner segment.
        -> (a -> Bool)  -- ^ Detect the start of an outer segment.
        -> (a -> Bool)  -- ^ Detect the end   of an outer segment.
        -> Vector r a   -- ^ Vector to dice.
        -> Vector UN (Vector UN (Vector r a))

dice pStart1 pEnd1 pStart2 pEnd2 !arr
 = let  lenArr           = size (extent arr)

        -- Do the inner segmentation.
        (starts1, lens1) = U.findSegments pStart1 pEnd1 
                         $ U.generate lenArr (\ix -> index arr (Z :. ix))

        -- To do the outer segmentation we want to check if the first
        -- and last characters in each of the inner segments match
        -- the predicates.
        pStart2' arr'    = pStart2 $ index arr' (ix1 0)
        pEnd2'   arr'    = pEnd2   $ index arr' (ix1 (size (extent arr') - 1))

        -- Do the outer segmentation.
        !lenArrInner     = U.length starts1
        !arrInner        = UNArray starts1 lens1 arr
        (starts2, lens2) = U.findSegmentsFrom pStart2' pEnd2'
                                lenArrInner
                                (\ix -> index arrInner (Z :. ix))

   in   UNArray starts2 lens2 arrInner
{-# INLINE [1] dice #-}


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
diceOn  :: (U.Unbox a, Eq a, Bulk r DIM1 a, Window r DIM1 a)
        => a            -- ^ Terminating element for inner segments.
        -> a            -- ^ Terminating element for outer segments.
        -> Vector r a   -- ^ Vector to dice.
        -> Vector UN (Vector UN (Vector r a))

diceOn !xEndWord !xEndLine !arr
 =      dice    (const True) (\x -> x == xEndWord || x == xEndLine)
                (const True) (\x -> x == xEndLine)
                arr
{-# INLINE [1] diceOn #-}


---------------------------------------------------------------------------------------------------
-- | For each segment of a nested vector, trim elements off the start
--   and end of the segment that match the given predicate.
trims   :: Bulk r DIM1 a
        => (a -> Bool)
        -> Vector UN (Vector r a)
        -> Vector UN (Vector r a)

trims pTrim (UNArray starts lengths elems)
 = let
        loop_trimEnds !start !len 
         | len == 0     = (start, len)
         | pTrim (elems `index` (Z :. start + len - 1)) 
                        = loop_trimEnds   start (len - 1)
         | otherwise    = loop_trimStarts start len
        {-# INLINE loop_trimEnds #-}

        loop_trimStarts !start !len 
         | len == 0     = (start, len)
         | pTrim (elems `index` (Z :. start + len - 1)) 
                        = loop_trimStarts (start + 1) (len - 1)
         | otherwise    = (start, len)
        {-# INLINE loop_trimStarts #-}

        (starts', lengths')
                = U.unzip $ U.zipWith loop_trimEnds starts lengths

   in   UNArray starts' lengths' elems
{-# INLINE [1] trims #-}


-- | For each segment of a nested vector, trim elements off the end of 
--   the segment that match the given predicate.
trimEnds :: Bulk r DIM1 a
         => (a -> Bool)
         -> Vector UN (Vector r a)
         -> Vector UN (Vector r a)

trimEnds pTrim (UNArray starts lengths elems)
 = let
        loop_trimEnds !start !len 
         | len == 0     = 0
         | pTrim (elems `index` (Z :. start + len - 1)) 
                        = loop_trimEnds start (len - 1)
         | otherwise    = len
        {-# INLINE loop_trimEnds #-}

        lengths'        = U.zipWith loop_trimEnds starts lengths

   in   UNArray starts lengths' elems
{-# INLINE [1] trimEnds #-}


-- | For each segment of a nested vector, trim elements off the start of
--   the segment that match the given predicate.
trimStarts :: Bulk r DIM1 a
           => (a -> Bool)
           -> Vector UN (Vector r a)
           -> Vector UN (Vector r a)

trimStarts pTrim (UNArray starts lengths elems)
 = let
        loop_trimStarts !start !len 
         | len == 0     = (start, len)
         | pTrim (elems `index` (Z :. start + len - 1)) 
                        = loop_trimStarts (start + 1) (len - 1)
         | otherwise    = (start, len)
        {-# INLINE loop_trimStarts #-}

        (starts', lengths')
                = U.unzip $ U.zipWith loop_trimStarts starts lengths

   in   UNArray starts' lengths' elems
{-# INLINE [1] trimStarts #-}


---------------------------------------------------------------------------------------------------
-- | Ragged transpose of a triply nested array.
-- 
--   * This version is more efficient than plain `ragspose` as the operation
--     is done entirely on the segment descriptors of the nested arrays.
--
ragspose3 :: Vector UN (Vector UN (Vector r a)) 
          -> Vector UN (Vector UN (Vector r a))

ragspose3 (UNArray starts1 lengths1 (UNArray starts2 lengths2 elems))
 = let  
        startStops1       = U.zipWith (\s l -> (s, s + l)) starts1 lengths1
        (ixs', lengths1') = U.ratchet startStops1

        starts2'          = U.map (U.unsafeIndex starts2)  ixs'
        lengths2'         = U.map (U.unsafeIndex lengths2) ixs'

        starts1'          = U.unsafeInit $ U.scanl (+) 0 lengths1'

   in   UNArray starts1' lengths1' (UNArray starts2' lengths2' elems)
{-# NOINLINE ragspose3 #-}
--  NOINLINE Because the operation is entirely on the segment descriptor.
--           This function won't fuse with anything externally, 
--           and it does not need to be specialiased.

