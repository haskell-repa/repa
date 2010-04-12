{-# LANGUAGE TypeOperators, FlexibleInstances #-}

module Data.Array.Repa.Index
	( Z	(..)
	, (:.)	(..)
	, DIM0
	, DIM1
	, DIM2
	, DIM3
	, DIM4
	, DIM5 )
where
import Test.QuickCheck


-- | An index of dimension zero
data Z	= Z

-- | Our index type, used for both shapes and indices.
infixl 3 :.
data tail :. head
	= tail :. head
	deriving (Show, Eq, Ord)

-- Common dimensions
type DIM0	= Z
type DIM1	= DIM0 :. Int
type DIM2	= DIM1 :. Int
type DIM3	= DIM2 :. Int
type DIM4	= DIM3 :. Int
type DIM5	= DIM4 :. Int


-- Instances --------------------------------------------------------------------------------------
instance Arbitrary sh => Arbitrary (sh :. Int) where
	arbitrary 
	 = do	h	<- arbitrary
		i	<- arbitrary
		return	$ h :. i