{-# LANGUAGE PatternGuards, PackageImports, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

-- | See the repa-examples package for examples.
--   
--   More information at <http://trac.haskell.org/repa>
--  
module Data.Array.Repa
	( module Data.Array.Repa.Elt
	, module Data.Array.Repa.Shape
	, module Data.Array.Repa.Index
	, module Data.Array.Repa.Slice

	-- from Data.Array.Repa.Base --------------------------------
	, Array(..)
	, deepSeqArray, deepSeqArrays
	, singleton,    toScalar
	, extent,       delay

	-- * Indexing
	, (!),  index
	, (!?), safeIndex
	, unsafeIndex

	-- * Conversions 
	, fromFunction	
	, fromVector, toVector
	, fromList,   toList
	
	-- * Forcing
	, force
	, forceBlockwise

	-- from Data.Array.Repa.Operators.IndexSpace ----------------
	-- * Index space transformations
	, reshape
	, append, (+:+)
	, transpose
	, replicate
	, slice
	, backpermute
	, backpermuteDft

	-- from Data.Array.Repa.Operators.Mapping -------------------
        -- * Structure preserving operations
	, map
	, zipWith
	, (+^), (-^), (*^), (/^)

	-- from Data.Array.Repa.Operators.Reduction -----------------
	-- * Reductions
	, fold,	foldAll
	, sum,	sumAll

	-- from Data.Array.Repa.Operators.Traverse ------------------
	-- * Generic Traversal
	, traverse,  unsafeTraverse
	, traverse2, unsafeTraverse2
	, traverse3, unsafeTraverse3
	, traverse4, unsafeTraverse4

	-- from Data.Array.Repa.Operators.Interleave ----------------
	-- * Interleaving
	, interleave2
	, interleave3
	, interleave4)
		
where
import Data.Array.Repa.Elt
import Data.Array.Repa.Index
import Data.Array.Repa.Slice
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Operators.Traverse
import Data.Array.Repa.Operators.IndexSpace
import Data.Array.Repa.Operators.Interleave
import Data.Array.Repa.Operators.Mapping
import Data.Array.Repa.Operators.Reduction
import qualified Data.Array.Repa.Shape	as S

import Prelude				hiding (sum, map, zipWith, replicate)	
import qualified Prelude		as P

stage	= "Data.Array.Repa"


-- Instances --------------------------------------------------------------------------------------
-- Show
instance (Shape sh, Elt a, Show a) => Show (Array sh a) where
 	show arr = show $ toList arr


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
	fromInteger n	 = Delayed failShape (\_ -> fromInteger n) 
	 where failShape = error $ stage ++ ".fromInteger: Constructed array has no shape."

