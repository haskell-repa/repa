
-- | Repa arrays are wrappers around a linear structure that holds the element data.
--   The representation tag determines what structure holds the data.
--
--   Delayed Representations (functions that compute elements)
--
--   * `D`  -- Functions from indices to elements.
--
--   * `C`  -- Cursor functions.
--
--   Manifest Representations (real data)
--
--   * `U`  -- Adaptive unboxed vectors.
--
--   * `B`  -- Strict ByteStrings. (TODO)
--
--   * `F`  -- Foreign memory buffers. (TODO)
--
--   * `L`  -- Haskell cons-lists.
--
--
--   Meta Representations
--
--   * `P`  -- Arrays that are partitioned into several representations.
--
--   * `X`  -- Arrays whose elements are all undefined.
--
--  Array fusion is achieved via the delayed (`D`) and cursored (`C`) representations. 
--  At compile time, the GHC simplifier combines the functions contained within `D` and `C` 
--  arrays without needing to create manifest intermediate arrays. 
--
--  Converting between the parallel manifest representations (eg `U` and `B`) is either
--  constant time or parallel copy, depending on the compatability of the physical representation.
--
module Data.Array.Repa
        ( -- * Array representation
          Array(..)
        , module Data.Array.Repa.Shape
        , module Data.Array.Repa.Index
        , Repr(..)
        , deepSeqArrays

        -- * Loading between representations
        , Load(..)

        -- * Representations
        -- ** Delayed representation
        , D, fromFunction, toFunction
        , delay, copy

        -- ** Unboxed vector representation
        , U, fromUnboxed, toUnboxed
        , force

        -- ** List representation
        , L, fromList, toList
                
	-- from Data.Array.Repa.Operators.IndexSpace ----------------
        -- * Operators
	-- ** Index space transformations
	, reshape
	, append, (++)
	, transpose
	, extend
	, backpermute
	, backpermuteDft

	, module Data.Array.Repa.Slice
	, slice

	-- from Data.Array.Repa.Operators.Mapping -------------------
        -- ** Structure preserving operations
	, map
	, zipWith
	, (+^), (-^), (*^), (/^)

	-- from Data.Array.Repa.Operators.Traversal ------------------
	-- ** Generic Traversal
	, traverse
	, traverse2
	, traverse3
	, traverse4
	, unsafeTraverse
	, unsafeTraverse2
	, unsafeTraverse3
	, unsafeTraverse4)        
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import Data.Array.Repa.Index
import Data.Array.Repa.Slice
import Data.Array.Repa.Repr.Undefined
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.List
import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa.Repr.Partitioned
import Data.Array.Repa.Repr.Cursored
import Data.Array.Repa.Operators.Mapping
import Data.Array.Repa.Operators.Traversal
import Data.Array.Repa.Operators.IndexSpace
import Prelude ()
