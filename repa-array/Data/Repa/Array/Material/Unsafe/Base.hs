
module Data.Repa.Array.Material.Unsafe.Base
        ( B (..)
        , U (..)
        , F (..)
        , N (..))
where


-- | Representation tag for Unsafe arrays of Boxed elements.
data B = B


-- | Representation tag for Unsafe arrays of Unboxed elements.
--
--   The implementation uses @Data.Vector.Unboxed@ which is based on type
--   families and picks an efficient, specialised representation for every
--   element type. In particular, unboxed vectors of pairs are represented
--   as pairs of unboxed vectors.
--   This is the most efficient representation for numerical data.
--
--   UNSAFE: Indexing into this array is not bounds checked.
--
data U = U


-- | Representation tag for Unsafe Foreign arrays.
data F = F


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
data N = N
