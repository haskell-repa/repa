-- Generic Traversal
module Data.Array.Repa.Operators.Traversal
        (traverse, unsafeTraverse)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import Data.Array.Repa.Repr.Delayed


-- | Unstructured traversal.
traverse
	:: forall r sh sh' a b
	.  (Shape sh, Shape sh', Repr r a)
	=> Array r sh a		        -- ^ Source array.
	-> (sh  -> sh')			-- ^ Function to produce the extent of the result.
	-> ((sh -> a) -> sh' -> b)	-- ^ Function to produce elements of the result.
	 				--   It is passed a lookup function to get elements of the source.
	-> Array D sh' b


{-# INLINE traverse #-}
traverse arr transExtent newElem
 	= fromFunction (transExtent (extent arr)) (newElem (index arr))


{-# INLINE unsafeTraverse #-}
unsafeTraverse arr transExtent newElem
 	= fromFunction (transExtent (extent arr)) (newElem (unsafeIndex arr))


