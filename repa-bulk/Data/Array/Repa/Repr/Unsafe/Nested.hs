
module Data.Array.Repa.Repr.Unsafe.Nested
        ( UN, U.Unbox
        , Array (..)
        , fromLists
        , fromListss
        , trims
        , segment, segmentOn
        , dice,    diceOn)
where
import Data.Array.Repa.Bulk.Base
import Data.Array.Repa.Bulk.Target
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.Window
import Data.Array.Repa.Shape
import Data.Array.Repa.Index
import Prelude                                          as P
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Array.Repa.Vector.Unboxed         as U
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
--   of four flat unboxed vectors: two for each of the segment descriptors
--   associated with each level of nesting, and one unboxed vector to hold
--   all the integer elements.
--
data UN

instance ( Bulk   r DIM1 a 
         , Window r DIM1 a)
      => Bulk UN DIM1 (Vector r a) where

 data Array UN DIM1 (Vector r a)
        = UNArray 
                 !(U.Vector Int)         -- segment start positions.
                 !(U.Vector Int)         -- segment lengths.
                 !(Array r DIM1 a)       -- data values

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
instance Window UN DIM1 (Vector r a) where
 window (Z :. start) (Z :. len) (UNArray starts lengths elems)
        = UNArray (U.unsafeSlice start len starts)
                  (U.unsafeSlice start len lengths)
                  elems
 {-# INLINE window #-}


-- Conversion -------------------------------------------------------------------------------------
-- | O(size src) Convert some lists to a nested array.
fromLists 
        :: Target r a 
        => [[a]] -> Vector UN (Vector r a)
fromLists xss
 = let  xs         = concat xss
        Just elems = fromList      (Z :. length xs) xs
        lengths    = U.fromList    $ map P.length xss
        starts     = U.unsafeInit  $ U.scanl (+) 0 lengths
   in   UNArray starts lengths elems
{-# INLINE [1] fromLists #-}
        

-- | O(size src) Convert a triply nested list to a triply nested array.
fromListss 
        :: Target r a 
        => [[[a]]] -> Vector UN (Vector UN (Vector r a))
fromListss xs
 = let  xs1        = concat xs
        xs2        = concat xs1
        Just elems = fromList (Z :. length xs2) xs2
        
        lengths1   = U.fromList   $ map P.length xs
        starts1    = U.unsafeInit $ U.scanl (+) 0 lengths1

        lengths2   = U.fromList   $ map P.length xs1
        starts2    = U.unsafeInit $ U.scanl (+) 0 lengths2

   in   UNArray    starts1 lengths1 
         $ UNArray starts2 lengths2 
         $ elems
{-# INLINE [1] fromListss #-}


---------------------------------------------------------------------------------------------------
-- | For each sub-array, if the last element matches the given predicate
--   then trim it off.
trims   :: Bulk r DIM1 a
        => (a -> Bool)
        -> Vector UN (Vector r a)
        -> Vector UN (Vector r a)

trims pTrim (UNArray starts lengths elems)
 = let
        ftrim start len 
         | len == 0     = 0
         | pTrim (elems `index` (Z :. start + len - 1)) 
                        = len - 1
         | otherwise    = len

        lengths'        = U.zipWith ftrim starts lengths
   in   UNArray starts lengths' elems
{-# INLINE [1] trims #-}


---------------------------------------------------------------------------------------------------
-- | Given predicates which detect the start and end of a segment, 
--   split an array into the indicated segments.
segment :: (U.Unbox a, Bulk r DIM1 a)
        => (a -> Bool)  -> (a -> Bool)          
        -> Vector r a    -> Vector UN (Vector r a)  

segment pStart pEnd !elems
 = let  len     = size (extent elems)
        (starts, lens)  
                = U.findSegments pStart pEnd 
                $ U.generate len (\ix -> index elems (Z :. ix))

        arr     = UNArray starts lens elems
   in   trims pEnd arr
{-# INLINE [1] segment #-}


-- | Split an array into segments, based on the given segment terminator element.
--
--   The result segments do not include the terminator.
--  
--   @segmentOn ' ' "fresh fried fish" = ["fresh", "fried", "fish"]@
--
segmentOn 
        :: (Eq a, U.Unbox a, Bulk r DIM1 a)
        => a
        -> Vector r a    -> Vector UN (Vector r a)

segmentOn !x !arr
 = segment (const True) (== x) arr
{-# INLINE [1] segmentOn #-}


---------------------------------------------------------------------------------------------------
-- | Like `segment`, but cut the source array twice.
--   TODO: dies with empty lines on end.
dice    :: (U.Unbox a, Bulk r DIM1 a, Window r DIM1 a)
        => (a -> Bool) -> (a -> Bool)         
        -> (a -> Bool) -> (a -> Bool)
        -> Vector r a
        -> Vector UN (Vector UN (Vector r a))

dice pStart1 pEnd1 pStart2 pEnd2 !arr
 = let  lenArr           = size (extent arr)
        (starts1, lens1) = U.findSegments pStart1 pEnd1 
                         $ U.generate lenArr (\ix -> index arr (Z :. ix))

        ahead arr1       = index arr1 (ix1 0)
        alast arr1       = index arr1 (ix1 (size (extent arr1) - 1))
        pStart2' arr     = pStart2 $ ahead arr
        pEnd2'   arr     = pEnd2   $ alast arr
        lenArrInner      = U.length starts1
        arrInner         = UNArray starts1 lens1 arr
        (starts2, lens2) = U.findSegmentsFrom pStart2' pEnd2'
                                lenArrInner
                                (\ix -> index arrInner (Z :. ix))

        pEndBoth x       = pEnd1 x || pEnd2 x
        arrInner'        = trims pEndBoth arrInner

   in   UNArray starts2 lens2 arrInner'
{-# INLINE [1] dice #-}


diceOn  :: (U.Unbox a, Eq a, Bulk r DIM1 a, Window r DIM1 a)
        => a -> a
        -> Vector r a    -> Vector UN (Vector UN (Vector r a))

diceOn !xEndWord !xEndLine !arr
 =      dice    (const True) (\x -> x == xEndWord || x == xEndLine)
                (const True) (\x -> x == xEndLine)
                arr
{-# INLINE [1] diceOn #-}

