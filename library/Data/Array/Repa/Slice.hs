{-# LANGUAGE TypeFamilies, TypeOperators #-}

module Data.Array.Repa.Slice
	( All	(..)
	, Any	(..))
where
-- import Data.Array.Repa.Index	

-- | Select all indices at a certain position.
data All 	= All

-- | ??? What is this for?
data Any sh	= Any sh

{-
class Slice ss where
	type FullShape  ss
	type SliceShape ss
	
	replicate 
		:: Elt e 
		=> ss 
		-> Array (SliceShape ss) e
		-> Array (FullShape  ss) e
		
	slice	:: Elt e
		=> Array (FullShape ss)  e
		-> ss
		-> Array (SliceShape ss) e
-}



