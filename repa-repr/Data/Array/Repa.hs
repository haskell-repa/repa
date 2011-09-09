
module Data.Array.Repa
        ( module Data.Array.Repa.Shape
        , module Data.Array.Repa.Index
        , module Data.Array.Repa.Slice

        -- * Basic types and functions
        , Array
        , Repr(..)
        , Load(..)
        , deepSeqArrays
        
        -- * Delayed representation
        , D, fromFunction, toFunction
        , delay, copy
                
        -- * Unboxed vector representation
        , U, fromUnboxed, toUnboxed
        , force
        
        -- * List representation
        , L, fromList, toList
        
	-- from Data.Array.Repa.Operators.IndexSpace ----------------
	-- * Index space transformations
	, reshape
	, append, (++)
	, transpose
	, extend
	, slice
	, backpermute
	, backpermuteDft

	-- from Data.Array.Repa.Operators.Mapping -------------------
        -- * Structure preserving operations
	, map
	, zipWith
	, (+^), (-^), (*^), (/^)

	-- from Data.Array.Repa.Operators.Traversal ------------------
	-- * Generic Traversal
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
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.List
import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa.Operators.Mapping
import Data.Array.Repa.Operators.Traversal
import Data.Array.Repa.Operators.IndexSpace
import Prelude ()
