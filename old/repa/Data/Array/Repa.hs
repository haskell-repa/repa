{-# LANGUAGE PatternGuards, PackageImports, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

-- | See the repa-examples package for examples.
--
--   More information at <http://repa.ouroborus.net>.
--
--   There is a draft tutorial at <http://www.haskell.org/haskellwiki/Numeric_Haskell:_A_Repa_Tutorial>
--
-- @Release Notes:
--  For 2.2.0.1:
--   * Added unsafeFromForeignPtr, which helps use foreign source
--     arrays without intermediate copying.
--   * Added forceWith and forceWith2, which can be used to force
--     arrays into foreign result buffers without intermediate copying.
--
--  For 2.1.0.1:
--   * The fold and foldAll functions now run in parallel and require the
--     starting element to be neutral with respect to the reduction operator.
--                                   -- thanks to Trevor McDonell
--   * Added (\/\/) update function.   -- thanks to Trevor McDonell
--   * Dropped unneeded Elt constraints from traverse functions.
-- @
module Data.Array.Repa
	( module Data.Array.Repa.Shape
	, module Data.Array.Repa.Index
	, module Data.Array.Repa.Slice

	-- from Data.Array.Repa.Internals.Elt -----------------------
	, Elt(..)

	-- from Data.Array.Repa.Internals.Base ----------------------
	, Array(..)
	, Region(..)
	, Range(..)
	, Rect(..)
	, Generator(..)
	, deepSeqArray, deepSeqArrays
	, singleton,    toScalar
	, extent,       delay

	--
	, withManifest, withManifest'

	-- * Indexing
	, (!),  index
	, (!?), safeIndex
	, unsafeIndex

	-- * Construction
	, fromFunction
	, fromVector
	, fromList
	, unsafeFromForeignPtr

	-- from Data.Array.Repa.Interlals.Forcing -------------------
	-- * Forcing
	, force,  forceWith
	, force2, forceWith2
	, toVector
	, toList

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

        -- from Data.Array.Repa.Operations.Modify -------------------
        -- * Bulk updates
        , (//)

	-- from Data.Array.Repa.Operators.Reduction -----------------
	-- * Reductions
	, fold,	foldAll
	, sum,	sumAll

	-- from Data.Array.Repa.Operators.Traverse ------------------
	-- * Generic Traversal
	, traverse
	, traverse2
	, traverse3
	, traverse4
	, unsafeTraverse
	, unsafeTraverse2
	, unsafeTraverse3
	, unsafeTraverse4

	-- from Data.Array.Repa.Operators.Interleave ----------------
	-- * Interleaving
	, interleave2
	, interleave3
	, interleave4

	-- from Data.Array.Repa.Operators.Select --------------------
	-- * Selection
	, select)

where
import Data.Array.Repa.Index
import Data.Array.Repa.Slice
import Data.Array.Repa.Shape
import Data.Array.Repa.Internals.Elt
import Data.Array.Repa.Internals.Base
import Data.Array.Repa.Internals.Forcing
import Data.Array.Repa.Operators.Traverse
import Data.Array.Repa.Operators.IndexSpace
import Data.Array.Repa.Operators.Interleave
import Data.Array.Repa.Operators.Mapping
import Data.Array.Repa.Operators.Modify
import Data.Array.Repa.Operators.Reduction
import Data.Array.Repa.Operators.Select
import qualified Data.Array.Repa.Shape	as S

import Prelude				hiding (sum, map, zipWith, (++))
import qualified Prelude		as P

stage	= "Data.Array.Repa"


-- Instances --------------------------------------------------------------------------------------
-- Show
instance (Shape sh, Elt a, Show a) => Show (Array sh a) where
        show arr =
          let shape = showShape (extent arr)
              elems = show      (toList arr)
          in
          "Array (" P.++ shape P.++ ") " P.++ elems


-- Eq
instance (Shape sh, Elt a, Eq a) => Eq (Array sh a) where

	{-# INLINE (==) #-}
	(==) arr1  arr2
		= foldAll (&&) True
		$ reshape (Z :. (S.size $ extent arr1))
		$ zipWith (==) arr1 arr2

	{-# INLINE (/=) #-}
	(/=) a1 a2
		= not $ (==) a1 a2

-- Num
-- All operators apply elementwise.
instance (Shape sh, Elt a, Num a) => Num (Array sh a) where
	{-# INLINE (+) #-}
	(+)		= zipWith (+)

	{-# INLINE (-) #-}
	(-)		= zipWith (-)

	{-# INLINE (*) #-}
	(*)		= zipWith (*)

	{-# INLINE negate #-}
	negate  	= map negate

	{-# INLINE abs #-}
	abs		= map abs

	{-# INLINE signum #-}
	signum 		= map signum

	{-# INLINE fromInteger #-}
	fromInteger n	 = fromFunction failShape (\_ -> fromInteger n)
	 where failShape = error $ stage P.++ ".fromInteger: Constructed array has no shape."


-- | Force an array before passing it to a function.
withManifest
	:: (Shape sh, Elt a)
	=> (Array sh a -> b) -> Array sh a -> b

{-# INLINE withManifest #-}
withManifest f arr
 = case arr of
	Array sh [Region RangeAll (GenManifest vec)]
	  -> vec `seq` f (Array sh [Region RangeAll (GenManifest vec)])

	_ -> f (force arr)


-- | Force an array before passing it to a function.
withManifest'
	:: (Shape sh, Elt a)
	=> Array sh a -> (Array sh a -> b) -> b

{-# INLINE withManifest' #-}
withManifest' arr f
 = case arr of
	Array sh [Region RangeAll (GenManifest vec)]
	 -> vec `seq` f (Array sh [Region RangeAll (GenManifest vec)])

	_ -> f (force arr)



