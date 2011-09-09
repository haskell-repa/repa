-- Generic Traversal
module Data.Array.Repa.Operators.Traversal
        ( traverse, unsafeTraverse
        , traverse2, unsafeTraverse2
	, traverse3, unsafeTraverse3
	, traverse4, unsafeTraverse4)
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
 = arr `deepSeqArray` 
   fromFunction (transExtent (extent arr)) (newElem (index arr))

{-# INLINE unsafeTraverse #-}
unsafeTraverse arr transExtent newElem
 = arr `deepSeqArray`
   fromFunction (transExtent (extent arr)) (newElem (unsafeIndex arr))


-- | Unstructured traversal over two arrays at once.
traverse2, unsafeTraverse2
	:: forall r1 r2 sh sh' sh'' a b c
	.  ( Shape sh,  Shape sh', Shape sh''
	   , Repr r1 a, Repr r2 b)
        => Array r1 sh  a 		-- ^ First source array.
	-> Array r2 sh' b		-- ^ Second source array.
        -> (sh -> sh' -> sh'')		-- ^ Function to produce the extent of the result.
        -> ((sh -> a) -> (sh' -> b)
                      -> (sh'' -> c))	-- ^ Function to produce elements of the result.
					--   It is passed lookup functions to get elements of the
					--   source arrays.
        -> Array D sh'' c

{-# INLINE traverse2 #-}
traverse2 arrA arrB transExtent newElem
 = arrA `deepSeqArray` arrB `deepSeqArray`
   fromFunction  (transExtent (extent arrA) (extent arrB))
 	         (newElem     (index  arrA) (index  arrB))

{-# INLINE unsafeTraverse2 #-}
unsafeTraverse2 arrA arrB transExtent newElem
 = arrA `deepSeqArray` arrB `deepSeqArray`
   fromFunction  (transExtent (extent arrA) (extent arrB))
                 (newElem     (unsafeIndex arrA) (unsafeIndex arrB))


-- | Unstructured traversal over three arrays at once.
traverse3, unsafeTraverse3
	:: forall r1  r2  r3
	          sh1 sh2 sh3 sh4
	          a   b   c   d
	.  ( Shape sh1, Shape sh2, Shape sh3, Shape sh4
	   , Repr r1 a, Repr r2 b, Repr r3 c)
        => Array r1 sh1 a
	-> Array r2 sh2 b
	-> Array r3 sh3 c
        -> (sh1 -> sh2 -> sh3 -> sh4)
        -> (  (sh1 -> a) -> (sh2 -> b)
           -> (sh3 -> c)
           ->  sh4 -> d )
        -> Array D sh4 d

{-# INLINE traverse3 #-}
traverse3 arrA arrB arrC transExtent newElem
 = arrA `deepSeqArray` arrB `deepSeqArray` arrC `deepSeqArray`
   fromFunction (transExtent (extent arrA) (extent arrB) (extent arrC))
 	        (newElem     (index arrA)  (index arrB)  (index  arrC))

{-# INLINE unsafeTraverse3 #-}
unsafeTraverse3 arrA arrB arrC transExtent newElem
 = arrA `deepSeqArray` arrB `deepSeqArray` arrC `deepSeqArray`
   fromFunction	(transExtent (extent arrA) (extent arrB) (extent arrC))
	        (newElem     (unsafeIndex arrA) (unsafeIndex arrB) (unsafeIndex arrC))


-- | Unstructured traversal over four arrays at once.
traverse4, unsafeTraverse4
	:: forall r1  r2  r3  r4
	          sh1 sh2 sh3 sh4 sh5
	          a   b   c   d   e
	.  ( Shape sh1, Shape sh2, Shape sh3, Shape sh4, Shape sh5
	   , Repr r1 a, Repr r2 b, Repr r3 c, Repr r4 d)
        => Array r1 sh1 a
	-> Array r2 sh2 b
	-> Array r3 sh3 c
	-> Array r4 sh4 d
        -> (sh1 -> sh2 -> sh3 -> sh4 -> sh5 )
        -> (  (sh1 -> a) -> (sh2 -> b)
           -> (sh3 -> c) -> (sh4 -> d)
           ->  sh5 -> e )
        -> Array D sh5 e

{-# INLINE traverse4 #-}
traverse4 arrA arrB arrC arrD transExtent newElem
 = arrA `deepSeqArray` arrB `deepSeqArray` arrC `deepSeqArray` arrD `deepSeqArray`
   fromFunction	(transExtent (extent arrA) (extent arrB) (extent arrC) (extent arrD))
		(newElem     (index  arrA) (index  arrB) (index  arrC) (index  arrD))


{-# INLINE unsafeTraverse4 #-}
unsafeTraverse4 arrA arrB arrC arrD transExtent newElem
 = arrA `deepSeqArray` arrB `deepSeqArray` arrC `deepSeqArray` arrD `deepSeqArray`
   fromFunction (transExtent (extent arrA) (extent arrB) (extent arrC) (extent arrD))
		(newElem     (unsafeIndex arrA) (unsafeIndex arrB) (unsafeIndex arrC) (unsafeIndex arrD))


