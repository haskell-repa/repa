{-# LANGUAGE TypeFamilies, TypeOperators #-}

module Data.Array.Repa.Slice
	( All	(..)
	, Any	(..)
	, Slice	(..))
where
import Data.Array.Repa.Array
import qualified Data.Array.Parallel.Unlifted	as U

-- | Select all indices at a certain position.
data All 	= All

-- | ??? What is this for?
data Any sh	= Any sh


class Slice ss where
	type FullShape  ss
	type SliceShape ss
	
	replicate 
		:: U.Elt e 
		=> ss 
		-> Array (SliceShape ss) e
		-> Array (FullShape  ss) e
		
	slice	:: U.Elt e
		=> Array (FullShape ss)  e
		-> ss
		-> Array (SliceShape ss) e




