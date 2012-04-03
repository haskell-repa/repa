
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
--   * `V`  -- Boxed vectors.
--
--   * `B`  -- Strict ByteStrings.
--
--   * `F`  -- Foreign memory buffers.
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
        ( -- * Abstract array representation
          Array(..)
        , module Data.Array.Repa.Shape
        , module Data.Array.Repa.Index
        , Repr(..), (!), toList
        , deepSeqArrays

        -- * Converting between array representations
        , computeP, computeS
        , copyP,    copyS
        , now

        -- * Concrete array representations
        -- ** Delayed representation
        , D, fromFunction, toFunction
        , delay

        -- ** Unboxed vector representation
        , U
        , computeUnboxedP, computeUnboxedS
        , fromListUnboxed
        , fromUnboxed
        , toUnboxed
                
	-- from Data.Array.Repa.Operators.IndexSpace ----------------
        -- * Operators
	-- ** Index space transformations
	, reshape
	, append, (++)
	, transpose
	, extend
	, backpermute,         unsafeBackpermute
	, backpermuteDft

	, module Data.Array.Repa.Slice
	, slice

	-- from Data.Array.Repa.Operators.Mapping -------------------
        -- ** Structure preserving operations
	, map
	, zipWith
        , zipWith'
	, (+^), (-^), (*^), (/^)
        , Combine(..)

	-- from Data.Array.Repa.Operators.Traversal ------------------
	-- ** Generic traversal
	, traverse,            unsafeTraverse
	, traverse2,           unsafeTraverse2
	, traverse3,           unsafeTraverse3
	, traverse4,           unsafeTraverse4
	
	-- from Data.Array.Repa.Operators.Interleave -----------------
	-- ** Interleaving
	, interleave2
	, interleave3
	, interleave4
	
	-- from Data.Array.Repa.Operators.Reduction ------------------
	-- ** Reduction
	, foldP,    foldS
	, foldAllP, foldAllS
	, sumP,     sumS
	, sumAllP,  sumAllS
	
	-- from Data.Array.Repa.Operators.Selection ------------------
	, select)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import Data.Array.Repa.Index
import Data.Array.Repa.Slice
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.Vector
import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa.Repr.ByteString
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.Cursored
import Data.Array.Repa.Repr.Partitioned
import Data.Array.Repa.Repr.Undefined           ()
import Data.Array.Repa.Operators.Mapping
import Data.Array.Repa.Operators.Traversal
import Data.Array.Repa.Operators.IndexSpace
import Data.Array.Repa.Operators.Interleave
import Data.Array.Repa.Operators.Reduction
import Data.Array.Repa.Operators.Selection
import Prelude          ()



