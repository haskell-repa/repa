
-- | Default Repa Array API that automatically choses an array layout based
--   on the element type.
module Data.Repa.Array.Default
        ( Array
        , Elem, Build

        -- * Basics
        , index
        , (!)
        , length

        -- * Conversion
        , fromList
        , fromLists
        , fromListss

        , toList
        , toLists
        , toListss

        -- * Operators
        -- ** Index space
        , reverse

        -- ** Mapping
        , map
        , map2

        -- ** Merging
        , merge
        , mergeMaybe

        -- ** Compacting
        , compact
        , compactIn

        -- ** Filtering
        , filter

        -- ** Inserting
        , insert

        -- ** Searching
        , findIndex

        -- ** Sloshing
        , concat
        , concatWith
        , intercalate
        , unlines

        -- ** Grouping
        , groups
        , groupsWith

        -- ** Folding
        -- ** Complete Fold
        , foldl
        , sum,  prod
        , mean, std
        , correlate

        -- ** Segmented Fold
        , folds
        , foldsWith
        )
where
import Data.Repa.Array.Material.Auto                    (A(..), Name(..))
import Control.Monad
import qualified Data.Repa.Array.Generic                as G
import qualified Data.Repa.Array.Material.Auto          as A
import qualified Data.Repa.Array.Material.Nested        as N
import qualified Data.Repa.Array.Internals.Bulk         as G
import qualified Data.Repa.Array.Internals.Target       as G
import qualified Data.Repa.Fusion.Unpack                as F
import qualified Data.Repa.Chain                        as C
import Prelude 
       hiding (map, length, reverse, filter, concat, unlines, foldl, sum)


-- | Arrays of elements using an automatic layout.
type Array a    =  G.Array A a

-- | Class of elements that can be automatically organised into arrays.
type Elem  a    
        = ( G.Bulk  A a
          , G.Windowable A a)

-- | Class of elements of which arrays of those elements can be built
--   in parallel.
type Build a t
        = ( G.Bulk   A a
          , G.Target A a
          , F.Unpack (G.IOBuffer A a) t)


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


-- Conversion -------------------------------------------------------------------------------------
-- | Convert a list to an array.
fromList :: Build a at 
         => [a] -> Array a
fromList = G.fromList A
{-# INLINE fromList #-}


-- | Convert a nested list to an array.
fromLists :: Build a at
          => [[a]] -> Array (Array a)
fromLists xs = A.AArray_Array (N.fromLists A xs)
{-# INLINE fromLists #-}


-- | Convert a triply nested list to a triply nested array.
fromListss :: Build a at
           => [[[a]]] -> Array (Array (Array a))
fromListss xs 
        = A.AArray_Array
        $ N.mapElems A.AArray_Array
        $ N.fromListss A xs
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
reverse arr = G.computeS A $ G.reverse arr
{-# INLINE reverse #-}


-- Mapping ----------------------------------------------------------------------------------------
-- | Apply a function to all the elements of a list.
map     :: (Elem a, Build b bt)
        => (a -> b) -> Array a -> Array b
map f arr
 = G.computeS A $! G.map f arr
{-# INLINE map #-}


-- | Combine two arrays of the same length element-wise.
map2    :: (Elem a, Elem b, Build c ct)
        => (a -> b -> c) -> Array a -> Array b -> Maybe (Array c)
map2 f xs ys
 = liftM (G.computeS A) $! G.map2 f xs ys
{-# INLINE map2 #-}


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
        => (s -> a -> (Maybe b, s))
        -> s
        -> Array a
        -> Array b
compact = G.compact A
{-# INLINE compact #-}


-- | Like `compact` but use the first value of the stream as the 
--   initial state, and add the final state to the end of the output.
compactIn
        :: Build a at
        => (a -> a -> (Maybe a, a))
        -> Array a
        -> Array a
compactIn = G.compactIn A
{-# INLINE compactIn #-}


-- Filtering --------------------------------------------------------------------------------------
-- | Keep the elements of an array that match the given predicate.
filter  :: Build a at
        => (a -> Bool) -> Array a -> Array a
filter = G.filter A
{-# INLINE filter #-}


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
prod   :: (Elem a, Num a) => Array a -> a
prod   = G.prod
{-# INLINE prod #-}


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



