{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ExplicitForAll #-}

module Data.Array.Repa.Operators.Traverse
	( traverse,  unsafeTraverse
	, traverse2, unsafeTraverse2
	, traverse3, unsafeTraverse3
	, traverse4, unsafeTraverse4)
where
import Data.Array.Repa.Internals.Elt
import Data.Array.Repa.Internals.Base
import Data.Array.Repa.Shape	as S

-- Generic Traversal -----------------------------------------------------------------------------
-- | Unstructured traversal.
traverse
	:: forall sh sh' a b
	.  (Shape sh, Shape sh', Elt a)
	=> Array sh a				-- ^ Source array.
	-> (sh  -> sh')				-- ^ Function to produce the extent of the result.
	-> ((sh -> a) -> sh' -> b)		-- ^ Function to produce elements of the result.
	 					--   It is passed a lookup function to get elements of the source.
	-> Array sh' b

{-# INLINE traverse #-}
traverse arr transExtent newElem
 	= arr `deepSeqArray`
          fromFunction (transExtent (extent arr)) (newElem (arr !))


{-# INLINE unsafeTraverse #-}
unsafeTraverse arr transExtent newElem
 	= arr `deepSeqArray`
	  fromFunction (transExtent (extent arr)) (newElem (unsafeIndex arr))


-- | Unstructured traversal over two arrays at once.
traverse2, unsafeTraverse2
	:: forall sh sh' sh'' a b c
	.  ( Shape sh, Shape sh', Shape sh''
	   , Elt a,    Elt b)
        => Array sh a 				-- ^ First source array.
	-> Array sh' b				-- ^ Second source array.
        -> (sh -> sh' -> sh'')			-- ^ Function to produce the extent of the result.
        -> ((sh -> a) -> (sh' -> b)
                      -> (sh'' -> c))		-- ^ Function to produce elements of the result.
						--   It is passed lookup functions to get elements of the
						--   source arrays.
        -> Array sh'' c

{-# INLINE traverse2 #-}
traverse2 arrA arrB transExtent newElem
	= arrA `deepSeqArray` arrB `deepSeqArray`
   	  fromFunction
		(transExtent (extent arrA) (extent arrB))
		(newElem     (arrA !) (arrB !))

{-# INLINE unsafeTraverse2 #-}
unsafeTraverse2 arrA arrB transExtent newElem
	= arrA `deepSeqArray` arrB `deepSeqArray`
   	  fromFunction
		(transExtent (extent arrA) (extent arrB))
		(newElem     (unsafeIndex arrA) (unsafeIndex arrB))


-- | Unstructured traversal over three arrays at once.
traverse3, unsafeTraverse3
	:: forall sh1 sh2 sh3 sh4
	          a   b   c   d
	.  ( Shape sh1, Shape sh2, Shape sh3, Shape sh4
	   , Elt a,     Elt b,     Elt c)
        => Array sh1 a
	-> Array sh2 b
	-> Array sh3 c
        -> (sh1 -> sh2 -> sh3 -> sh4)
        -> (  (sh1 -> a) -> (sh2 -> b)
           -> (sh3 -> c)
           ->  sh4 -> d )
        -> Array sh4 d

{-# INLINE traverse3 #-}
traverse3 arrA arrB arrC transExtent newElem
	= arrA `deepSeqArray` arrB `deepSeqArray` arrC `deepSeqArray`
   	  fromFunction
		(transExtent (extent arrA) (extent arrB) (extent arrC))
		(newElem     (arrA !) (arrB !) (arrC !))

{-# INLINE unsafeTraverse3 #-}
unsafeTraverse3 arrA arrB arrC transExtent newElem
	= arrA `deepSeqArray` arrB `deepSeqArray` arrC `deepSeqArray`
   	  fromFunction
		(transExtent (extent arrA) (extent arrB) (extent arrC))
		(newElem     (unsafeIndex arrA) (unsafeIndex arrB) (unsafeIndex arrC))


-- | Unstructured traversal over four arrays at once.
traverse4, unsafeTraverse4
	:: forall sh1 sh2 sh3 sh4 sh5
	          a   b   c   d   e
	.  ( Shape sh1, Shape sh2, Shape sh3, Shape sh4, Shape sh5
	   , Elt a,     Elt b,     Elt c,     Elt d)
        => Array sh1 a
	-> Array sh2 b
	-> Array sh3 c
	-> Array sh4 d
        -> (sh1 -> sh2 -> sh3 -> sh4 -> sh5 )
        -> (  (sh1 -> a) -> (sh2 -> b)
           -> (sh3 -> c) -> (sh4 -> d)
           ->  sh5 -> e )
        -> Array sh5 e

{-# INLINE traverse4 #-}
traverse4 arrA arrB arrC arrD transExtent newElem
	= arrA `deepSeqArray` arrB `deepSeqArray` arrC `deepSeqArray` arrD `deepSeqArray`
   	  fromFunction
		(transExtent (extent arrA) (extent arrB) (extent arrC) (extent arrD))
		(newElem     (arrA !) (arrB !) (arrC !) (arrD !))


{-# INLINE unsafeTraverse4 #-}
unsafeTraverse4 arrA arrB arrC arrD transExtent newElem
	= arrA `deepSeqArray` arrB `deepSeqArray` arrC `deepSeqArray` arrD `deepSeqArray`
   	  fromFunction
		(transExtent (extent arrA) (extent arrB) (extent arrC) (extent arrD))
		(newElem     (unsafeIndex arrA) (unsafeIndex arrB) (unsafeIndex arrC) (unsafeIndex arrD))

