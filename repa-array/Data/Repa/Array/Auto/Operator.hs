
-- | Repa Array API that automatically choses an array layout based
--   on the element type.
--
--   This is a re-export of the module "Data.Repa.Array".
--
module Data.Repa.Array.Auto.Operator
        ( Array 
        , Elem, Build

        -- * Basics
        , index
        , (!)
        , length
        , first, last
        , head,  tail, init

        -- * Construction
        , empty
        , singleton
        , generateMaybeS,  mapMaybeS
        , generateEitherS, mapEitherS

        -- * Conversion
        , fromList
        , fromLists
        , fromListss

        , toList
        , toLists
        , toListss

        -- * Operators

        -- ** Replicating
        , replicates

        -- ** Mapping
        , map
        , map2
        , mapElems

        -- ** Folding
        , foldl
        , sum,  product
        , mean, std

        -- *** Special Folds
        , correlate
        , folds
        , foldsWith

        -- ** Filtering
        , filter
        , slices
        , trims
        , trimEnds
        , trimStarts

        -- ** Zipping
        , zip
        , unzip

        -- ** Sloshing
        , reverse
        , concat
        , concats
        , concatWith
        , unlines
        , intercalate
        , ragspose3

        -- ** Slicing
        , slice

        -- ** Inserting
        , insert

        -- ** Searching
        , findIndex

        -- ** Merging
        , merge
        , mergeMaybe

        -- ** Compacting
        , compact
        , compactIn

        -- ** Processing
        , process

        -- ** Grouping
        , groups
        , groupsWith

        -- ** Splitting
        , segment
        , segmentOn
        , dice
        , diceSep)
where
import Data.Repa.Array.Auto.Base
import Data.Repa.Array.Material.Auto                    (Name(..))
import Data.Repa.Array.Generic.Convert                  as A
import Control.Monad
import GHC.Exts                                         hiding (fromList, toList)
import qualified Data.Repa.Array.Generic                as G
import qualified Data.Repa.Array.Material.Auto          as A
import qualified Data.Repa.Array.Material.Nested        as N
import qualified Data.Repa.Array.Meta.Tuple             as A
import qualified Data.Repa.Array.Meta.Window            as A
import qualified Data.Repa.Array.Meta.Delayed           as A
import qualified Data.Repa.Array.Meta.Delayed2          as A
import qualified Data.Repa.Array.Internals.Bulk         as G
import qualified Data.Repa.Fusion.Unpack                as F
import qualified Data.Repa.Chain                        as C
import qualified Data.Vector.Unboxed                    as U
import Prelude 
       hiding   ( map,  length, reverse, filter, concat, unlines, foldl
                , sum,  product, zip, unzip
                , head, tail, init, last)


-- Basic ------------------------------------------------------------------------------------------
-- | O(1). Get an element from an array. 
--
--   If the provided index is outside the extent of the array then the
--   result depends on the layout.
index :: Elem a => Array a -> Int -> a
index  = (G.!)
{-# INLINE index #-}


-- | O(1). Alias for `index`
(!) :: Elem a => Array a -> Int -> a
(!)    = index
{-# INLINE (!) #-}


-- | O(1). Get the number of elements in an array.
length :: Elem a => Array a -> Int
length = G.length
{-# INLINE length #-}


-- | O(1). Take the first element of an array, or `Nothing` if it's empty.
first   :: Elem a => Array a -> Maybe a
first   = G.first
{-# INLINE first #-}


-- | O(1). Take the last element of an array, or `Nothing` if it's empty.
last    :: Elem a => Array a -> Maybe a
last    = G.last
{-# INLINE last #-}


-- | O(1). alias for `first`.
head    :: Elem a => Array a -> Maybe a
head    = G.first
{-# INLINE head #-}


-- | O(1). Take the tail of an array, or `Nothing` if it's empty.
tail    :: Elem a => Array a -> Maybe (Array a)
tail    = A.tail
{-# INLINE tail #-}


-- | O(1). Take the initial elements of an array, or `Nothing` if it's empty.
init    :: Elem a => Array a -> Maybe (Array a)
init    = A.init
{-# INLINE init #-}


-- Construction -----------------------------------------------------------------------------------
-- | O(1). An empty array of the given layout.
empty   :: Build a at
        => Array a
empty = G.empty A
{-# INLINE empty #-}


-- | O(1). Create a new empty array containing a single element.
singleton 
        :: Build a at
        => a -> Array a
singleton = G.singleton A
{-# INLINE singleton #-}


-- | Like `generateS` but use a function that produces Maybe an element.
--   If any element returns `Nothing`, then `Nothing` for the whole array.
generateMaybeS
        :: Build a at
        => Int -> (Int -> Maybe a) 
        -> Maybe (Array a)
generateMaybeS = G.generateMaybeS A
{-# INLINE generateMaybeS #-}


-- | Apply a function to every element of an array, 
--   if any application returns `Nothing`, then `Nothing` for the whole result.
mapMaybeS 
        :: (Elem a, Build b bt)
        => (a -> Maybe b) 
        -> Array a
        -> Maybe (Array b)
mapMaybeS = G.mapMaybeS A
{-# INLINE mapMaybeS #-}


-- | Like `generateS` but use a function that produces Either some error
--   or an element. If any element returns `Nothing`, then `Nothing` for
--   the whole array.
generateEitherS
        :: Build a at
        => Int -> (Int -> Either err a) 
        -> Either err (Array a)
generateEitherS = G.generateEitherS A
{-# INLINE generateEitherS #-}


-- | Apply a function to every element of an array, 
--   if any application returns `Left`, then `Left` for the whole result.
mapEitherS 
        :: (Elem a, Build b bt)
        => (a -> Either err b) 
        -> Array a
        -> Either err (Array b)
mapEitherS = G.mapEitherS A
{-# INLINE mapEitherS #-}


-- Conversion -------------------------------------------------------------------------------------
-- | Convert a list to an array.
fromList :: Build a at 
         => [a] -> Array a
fromList = G.fromList A
{-# INLINE fromList #-}


-- | Convert a nested list to an array.
fromLists :: Build a at
          => [[a]] -> Array (Array a)
fromLists xs  = convert $! N.fromLists A xs
{-# INLINE fromLists #-}


-- | Convert a triply nested list to a triply nested array.
fromListss :: Build a at
           => [[[a]]] -> Array (Array (Array a))
fromListss xs = convert $! N.fromListss A xs
{-# INLINE fromListss #-}


-- | Convert an array to a list.
toList :: Elem a => Array a -> [a]
toList = G.toList
{-# INLINE toList #-}


-- | Convert a nested array to some lists.
toLists :: (Elem a, Elem (Array a)) 
        => Array (Array a) -> [[a]]
toLists = G.toLists
{-# INLINE toLists #-}


-- | Convert a triply nested array to a triply nested list.
toListss :: (Elem a, Elem (Array a), Elem (Array (Array a)))
         => Array (Array (Array a)) -> [[[a]]]
toListss = G.toListss
{-# INLINE toListss #-}


-- Index space ------------------------------------------------------------------------------------
-- | O(n). Reverse the elements of a list.
--
-- @
-- > toList $ reverse $ fromList [0 .. 10 :: Int]
-- [10,9,8,7,6,5,4,3,2,1,0]
-- @
--
reverse :: Build a at => Array a -> Array a
reverse arr = G.computeS A $! A.reverse arr
{-# INLINE reverse #-}


-- Replicating ------------------------------------------------------------------------------------
-- | Segmented replicate.
replicates 
        :: (Elem a, Build a at)
        => Array (Int, a) -> Array a

replicates arr
 = G.replicates A arr
{-# INLINE replicates #-}


-- Mapping ----------------------------------------------------------------------------------------
-- | Apply a function to all the elements of a list.
map     :: (Elem a, Build b bt)
        => (a -> b) -> Array a -> Array b
map f arr
 = G.computeS A $! A.map f arr
{-# INLINE map #-}


-- | Combine two arrays of the same length element-wise.
--
--   If the arrays don't have the same length then `Nothing`.
--
map2    :: (Elem a, Elem b, Build c ct)
        => (a -> b -> c) -> Array a -> Array b -> Maybe (Array c)
map2 f xs ys
 = liftM (G.computeS A) $! A.map2 f xs ys
{-# INLINE map2 #-}


-- | Apply a function to all the elements of a doubly nested
--   array, preserving the nesting structure. 
--
--   * This function has a non-standard time complexity.
--     As nested arrays use a segment descriptor based representation,
--     detatching and reattaching the nesting structure is a constant time
--     operation. However, the array passed to the worker function will
--     also contain any elements in the array representation that are 
--     not reachable from the segment descriptor. This matters if the 
--     source array was produced by a function that filters the segments
--     directly, like `slices`.
--
mapElems :: (Array a -> Array b)
         -> Array (Array a) -> (Array (Array b))
mapElems f (A.AArray_Array arr)
 = A.AArray_Array (N.mapElems f arr)
{-# INLINE mapElems #-}


-- Folding ----------------------------------------------------------------------------------------
-- | Left fold of all elements in an array.
foldl   :: Elem b
        => (a -> b -> a) -> a -> Array b -> a
foldl = G.foldl
{-# INLINE foldl #-}


-- | Yield the sum of the elements of an array.
sum    :: (Elem a, Num a) => Array a -> a
sum   = G.sum
{-# INLINE sum #-}


-- | Yield the product of the elements of an array.
product   :: (Elem a, Num a) => Array a -> a
product = G.product
{-# INLINE product #-}


-- | Yield the mean value of the elements of an array.
mean   :: (Elem a, Fractional a) 
        => Array a -> a
mean   = G.mean
{-# INLINE mean #-}


-- | Yield the standard deviation of the elements of an array
std    ::  (Elem a, Floating a)
        => Array a -> a
std     = G.std
{-# INLINE std #-}


-- | Compute the Pearson correlation of two arrays.
--
--   If the arrays differ in length then only the common
--   prefix is correlated.
--
correlate 
        :: ( Elem a, Floating a)
        => Array a -> Array a -> a
correlate = G.correlate
{-# INLINE correlate #-}


-- | Segmented fold over vectors of segment lengths and input values.
--
--   * The total lengths of all segments need not match the length of the
--     input elements vector. The returned `C.Folds` state can be inspected
--     to determine whether all segments were completely folded, or the
--     vector of segment lengths or elements was too short relative to the
--     other.
--
folds   :: (Elem a, Build n nt, Build b bt)
        => (a -> b -> b)        -- ^ Worker function.
        -> b                    -- ^ Initial state when folding segments.
        -> Array (n, Int)       -- ^ Segment names and lengths.
        -> Array a              -- ^ Elements.
        -> (Array (n, b), C.Folds Int Int n a b)
folds f z lens vals
 = let  (arr', result) = G.folds A A f z lens vals
   in   (A.AArray_T2 arr', result)
{-# INLINE folds #-}


-- | Like `folds`, but take an initial state for the first segment.
--
foldsWith
        :: (Elem a, Build n nt, Build b bt)
        => (a -> b -> b)         -- ^ Worker function.
        -> b                     -- ^ Initial state when folding segments.
        -> Maybe (n, Int, b)     -- ^ Name, length and initial state for first segment.
        -> Array (n, Int)        -- ^ Segment names and lengths.
        -> Array a               -- ^ Elements.
        -> (Array (n, b), C.Folds Int Int n a b)
foldsWith f z start lens vals
 = let  (arr', result)  = G.foldsWith A A f z start lens vals
   in   (A.AArray_T2 arr', result)
{-# INLINE foldsWith #-}


-- Filtering --------------------------------------------------------------------------------------
-- | O(len src) Keep the elements of an array that match the given predicate.
filter  :: Build a at
        => (a -> Bool) -> Array a -> Array a
filter = G.filter A
{-# INLINE filter #-}


-- | O(1). Produce a nested array by taking slices from some array of elements.
--   
--  * This is a constant time operation, as the representation for nested 
--    vectors just wraps the starts, lengths and elements vectors.
--
slices  :: Array Int            -- ^ Segment starting positions.
        -> Array Int            -- ^ Segment lengths.
        -> Array a              -- ^ Array elements.
        -> Array (Array a)

slices (A.AArray_Int starts) (A.AArray_Int lens) elems
        =  A.AArray_Array 
        $! N.slices starts lens elems
{-# INLINE slices #-}


-- | For each segment of a nested vector, trim elements off the start
--   and end of the segment that match the given predicate.
trims   :: Elem a
        => (a -> Bool)
        -> Array (Array a)
        -> Array (Array a)

trims f (A.AArray_Array arr)
        = A.AArray_Array $! N.trims f arr
{-# INLINE trims #-}


-- | For each segment of a nested array, trim elements off the end of 
--   the segment that match the given predicate.
trimEnds :: Elem a
         => (a -> Bool)
         -> Array (Array a)
         -> Array (Array a)

trimEnds f (A.AArray_Array arr)
        = A.AArray_Array $! N.trimEnds f arr
{-# INLINE trimEnds #-}


-- | For each segment of a nested array, trim elements off the start of
--   the segment that match the given predicate.
trimStarts :: Elem a
           => (a -> Bool)
           -> Array (Array a)
           -> Array (Array a)
trimStarts f (A.AArray_Array arr)
        = A.AArray_Array $! N.trimStarts f arr
{-# INLINE trimStarts #-}


-- Zipping ----------------------------------------------------------------------------------------
-- | O(1). Pack a pair of arrays to an array of pairs.
zip     :: (Elem a, Elem b) 
        => Array a -> Array b -> Array (a, b)
zip arr1 arr2
 = let  len     = max (length arr1) (length arr2)
        arr1'   = A.window 0 len arr1
        arr2'   = A.window 0 len arr2
   in   A.AArray_T2 (A.tup2 arr1' arr2')
{-# INLINE zip #-}


-- | O(1). Unpack an array of pairs to a pair of arrays.
unzip   :: (Elem a, Elem b)
        => Array (a, b) -> (Array a, Array b)
unzip arr@(A.AArray_T2 arr')
 = let  len     = length arr
        (arr1, arr2) = A.untup2 arr'
        arr1'   = A.window 0 len arr1
        arr2'   = A.window 0 len arr2
   in   (arr1', arr2')


-- Sloshing ---------------------------------------------------------------------------------------
-- | Concatenate nested arrays.
concat  :: (Elem a, Build a at, F.Unpack (Array a) aat)
        => Array (Array a)      -- ^ Arrays to concatenate.
        -> Array a
concat = G.concat A
{-# INLINE concat #-}


-- | O(len result) Concatenate the elements of some nested vector,
--   inserting a copy of the provided separator array between each element.
concatWith
        :: (Elem a, Build a at, F.Unpack (Array a) aat)
        => Array a              -- ^ Separator array.
        -> Array (Array a)      -- ^ Arrays to concatenate.
        -> Array a
concatWith = G.concatWith A
{-# INLINE concatWith #-}


-- | O(len result) Concatenate the outer two layers of a triply nested array.
--   (Segmented concatenation).
--
--   * The operation is performed entirely on the segment descriptors of the 
--     array, and does not require the inner array elements to be copied.
--   * This version is faster than plain `concat` on triply nested arrays.
--
concats :: Array (Array (Array a))
        -> Array (Array a)

concats (A.AArray_Array (N.NArray starts1 lens1 (A.AArray_Array arr)))
 = A.AArray_Array $ N.concats (N.NArray starts1 lens1 arr)
{-# INLINE concats #-}


-- | O(len result) Perform a `concatWith`, adding a newline character to
--   the end of each inner array.
unlines :: F.Unpack (Array Char) aat
        => Array (Array Char) -> Array Char
unlines = G.unlines A
{-# INLINE unlines #-}


-- | O(len result) Insert a copy of the separator array between the elements of
--   the second and concatenate the result.
intercalate 
        :: (Elem a, Build a at, F.Unpack (Array a) aat)
        => Array a              -- ^ Separator array.
        -> Array (Array a)      -- ^ Arrays to concatenate.
        -> Array a
intercalate = G.intercalate A
{-# INLINE intercalate #-}


-- | Ragged transpose of a triply nested array.
-- 
--   * This operation is performed entirely on the segment descriptors
--     of the nested arrays, and does not require the inner array elements
--     to be copied.
--
ragspose3 :: Array (Array (Array a)) 
          -> Array (Array (Array a))

ragspose3 (A.AArray_Array (N.NArray starts0 lens0 (A.AArray_Array arr)))
 = let  N.NArray starts1 elems1 (N.NArray starts2 elems2 arr')
                = N.ragspose3 (N.NArray starts0 lens0 arr)

   in   A.AArray_Array 
                $! N.NArray starts1 elems1
                $! A.AArray_Array 
                $! N.NArray starts2 elems2 arr'
{-# INLINE ragspose3 #-}


-- Slicing ----------------------------------------------------------------------------------------
-- | Take a slice out of an array, given a starting position and length.
slice   :: Elem a => Int -> Int -> Array a -> Maybe (Array a)
slice from len arr
        | from >= 0, len >= 0
        , len  <= G.length arr - from
        = Just $ A.window from len arr

        | otherwise
        = Nothing
{-# INLINE slice #-}


-- Merging ----------------------------------------------------------------------------------------
-- | Merge two sorted key-value streams.
merge   :: (Ord k, Elem (k, a), Elem (k, b), Build (k, c) ct)
        => (k -> a -> b -> c)   -- ^ Combine two values with the same key.
        -> (k -> a -> c)        -- ^ Handle a left value without a right value.
        -> (k -> b -> c)        -- ^ Handle a right value without a left value.
        -> Array (k, a)         -- ^ Array of keys and left values.
        -> Array (k, b)         -- ^ Array of keys and right values.
        -> Array (k, c)         -- ^ Array of keys and results.
merge = G.merge A
{-# INLINE merge #-}


-- | Like `merge`, but only produce the elements where the worker functions
--   return `Just`.
mergeMaybe 
        :: (Ord k, Elem (k, a), Elem (k, b), Build (k, c) ct)
        => (k -> a -> b -> Maybe c) -- ^ Combine two values with the same key.
        -> (k -> a -> Maybe c)      -- ^ Handle a left value without a right value.
        -> (k -> b -> Maybe c)      -- ^ Handle a right value without a left value.
        -> Array (k, a)             -- ^ Array of keys and left values.
        -> Array (k, b)             -- ^ Array of keys and right values.
        -> Array (k, c)             -- ^ Array of keys and results.
mergeMaybe = G.mergeMaybe A
{-# INLINE mergeMaybe #-}


-- Splitting --------------------------------------------------------------------------------------
-- | Combination of `fold` and `filter`. 
--   
--   We walk over the stream front to back, maintaining an accumulator.
--   At each point we can chose to emit an element (or not)
--
compact :: (Elem a, Build b bt)
        => (s -> a -> (s, Maybe b))
        -> s
        -> Array a
        -> Array b
compact = G.compact A
{-# INLINE compact #-}


-- | Like `compact` but use the first value of the stream as the 
--   initial state, and add the final state to the end of the output.
compactIn
        :: Build a at
        => (a -> a -> (a, Maybe a))
        -> Array a
        -> Array a
compactIn = G.compactIn A
{-# INLINE compactIn #-}


-- | Apply a generic stream process to an array.
process :: ( Build a at, Build b bt, Elem b
           , F.Unpack (G.Array A.A b) t
           , G.Target A.A b)
        => (s -> a -> (s, Array b))     -- ^ Worker function
        -> s                            -- ^ Initial state.
        -> Array a                      -- ^ Input array.
        -> (s, Array b)                 -- ^ Result state and array.
process   = G.process A
{-# INLINE process #-}


-- Inserting --------------------------------------------------------------------------------------
-- | Insert elements produced by the given function in to an array.
insert  :: Build a at
        => (Int -> Maybe a) -> Array a -> Array a
insert = G.insert A
{-# INLINE insert #-}


-- Searching --------------------------------------------------------------------------------------
-- | O(len src) Yield `Just` the index of the first element matching the predicate
--   or `Nothing` if no such element exists.
findIndex :: Elem a => (a -> Bool) -> Array a -> Maybe Int
findIndex = G.findIndex 
{-# INLINE findIndex #-}


-- Splitting --------------------------------------------------------------------------------------
-- | O(len src). Given predicates which detect the start and end of a segment, 
--   split an vector into the indicated segments.
segment :: (Elem a, U.Unbox a)
        => (a -> Bool)  -- ^ Detect the start of a segment.
        -> (a -> Bool)  -- ^ Detect the end of a segment.
        -> Array a      -- ^ Array to segment.
        -> Array (Array a)  

segment pStart pEnd elems
        = A.AArray_Array $! N.segment pStart pEnd elems
{-# INLINE segment #-}


-- | O(len src). Given a terminating value, split an vector into segments.
--
--   The result segments do not include the terminator.
segmentOn 
        :: (Elem a, Eq a, U.Unbox a)
        => (a -> Bool)  -- ^ Detect the end of a segment.
        -> Array a      -- ^ Array to segment.
        -> Array (Array a)

segmentOn pEnd arr
        = A.AArray_Array $! N.segmentOn pEnd arr
{-# INLINE segmentOn #-}


-- | O(len src). Like `segment`, but cut the source array twice.
dice    :: (Elem a, U.Unbox a)
        => (a -> Bool)  -- ^ Detect the start of an inner segment.
        -> (a -> Bool)  -- ^ Detect the end   of an inner segment.
        -> (a -> Bool)  -- ^ Detect the start of an outer segment.
        -> (a -> Bool)  -- ^ Detect the end   of an outer segment.
        -> Array a      -- ^ Array to dice.
        -> Array (Array (Array a))

dice pStart1 pEnd1 pStart2 pEnd2 arr
 = let  N.NArray starts1 elems1 (N.NArray starts2 elems2 arr')
                = N.dice pStart1 pEnd1 pStart2 pEnd2 arr
  
   in   A.AArray_Array 
                $! N.NArray starts1 elems1
                $! A.AArray_Array 
                $! N.NArray starts2 elems2 arr'
{-# INLINE dice #-}


-- | O(len src). Given field and row terminating values, 
--   split an array into rows and fields.
--
diceSep :: (Elem a, Eq a, U.Unbox a)
        => a            -- ^ Terminating element for inner segments.
        -> a            -- ^ Terminating element for outer segments.
        -> Array a      -- ^ Vector to dice.
        -> Array (Array (Array a))

diceSep xEndCol xEndRow arr
 = let  N.NArray starts1 elems1 (N.NArray starts2 elems2 arr')
                = N.diceSep xEndCol xEndRow arr
  
   in   A.AArray_Array 
                $! N.NArray starts1 elems1
                $! A.AArray_Array 
                $! N.NArray starts2 elems2 arr'
{-# INLINE diceSep #-}


-- Grouping ---------------------------------------------------------------------------------------
-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
groups  :: (Eq a, Build a at)
        => Array a              -- ^ Input elements.
        -> (Array (a, Int), Maybe (a, Int))
                                -- ^ Completed and final segment lengths.
groups arr
 = let  (arr', result) = G.groups A A arr
   in   (A.AArray_T2 arr', result)
{-# INLINE groups #-}


-- | Like `groups`, but use the given function to determine whether two
--   consecutive elements should be in the same group. 
--   Also take an initial starting group and count.
groupsWith
        :: Build a at
        => (a -> a -> Bool)     -- ^ Comparison function.
        -> Maybe  (a, Int)      -- ^ Starting element and count.
        -> Array  a             -- ^ Input elements.
        -> (Array (a, Int), Maybe (a, Int))     
                                -- ^ Completed and final segment lengths.
groupsWith f start arr
 = let  (arr', result) = G.groupsWith A A f start arr
   in   (A.AArray_T2 arr', result)
{-# INLINE groupsWith #-}


