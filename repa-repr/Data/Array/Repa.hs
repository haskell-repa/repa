
-- | Repa arrays are wrappers around a linear structure that holds the element data.
--   The representation tag determines what structure holds the data.
--
--   Currently supported are:
--
--   * `D`  -- Delayed arrays as functions from indices to elements.
--
--   * `U`  -- Arrays as adaptive unboxed vectors.
--
--   * `B`  -- Arrays as strict ByteStrings. (TODO)
--
--   * `F`  -- Arrays as foreign memory buffers. (TODO)
--
--   * `L`  -- Arrays as Haskell cons-lists.
--
--   * `P`  -- Arrays that are partitioned into several representations.
--
--   * `C`  -- Arrays as cursor functions. (TODO)
--
--   * `X`  -- Undefined arrays.
--
--  Array fusion is achieved via the delayed (`D`) and cursored (`C`) representations. 
--  At compile time, the GHC simplifier combines the functions contained within `D` and `C` 
--  arrays without needing to create manifest intermediate arrays. Converting between the
--  parallel manifest representations (eg `U` and `B`) is either constant time or parallel
--  copy, depending on the compatability of the physical representation.
--
module Data.Array.Repa
        ( -- * Array representation
          Array(..)
        , module Data.Array.Repa.Shape
        , module Data.Array.Repa.Index
        , module Data.Array.Repa.Slice
        , Repr(..)
        , deepSeqArrays

        -- * Parallel array filling
        , Fill (..)

        -- * Loading between representations
        , Load(..), Load2(..)

        -- * Representations
        -- ** Delayed representation
        , D, fromFunction, toFunction
        , delay, copy

        -- ** Unboxed vector representation
        , U, fromUnboxed, toUnboxed
        , force

        -- ** List representation
        , L, fromList, toList

        -- ** Partitioned array representation
        , P, Range(..), inRange

        -- ** Undefined arrays
        , X
                
	-- from Data.Array.Repa.Operators.IndexSpace ----------------
        -- * Operators
	-- ** Index space transformations
	, reshape
	, append, (++)
	, transpose
	, extend
	, slice
	, backpermute
	, backpermuteDft

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
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.Undefined
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.List
import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa.Repr.Partitioned
import Data.Array.Repa.Operators.Mapping
import Data.Array.Repa.Operators.Traversal
import Data.Array.Repa.Operators.IndexSpace
import Prelude ()
