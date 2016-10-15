-- Generic Traversal
module Data.Array.Repa.Operators.Traversal
        ( traverse,  unsafeTraverse
        , traverse2, unsafeTraverse2
        , traverse3, unsafeTraverse3
        , traverse4, unsafeTraverse4)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import Data.Array.Repa.Repr.Delayed
import Prelude hiding (traverse)


-- | Unstructured traversal.
traverse, unsafeTraverse
        :: forall r sh sh' a b
        .  ( Source r a
           , Shape  sh)
        => Array r sh a                 -- ^ Source array.
        -> (sh  -> sh')                 -- ^ Function to produce the extent of the result.
        -> ((sh -> a) -> sh' -> b)      -- ^ Function to produce elements of the result.
                                        --   It is passed a lookup function to get elements of the source.
        -> Array D sh' b

traverse arr transExtent newElem
 = fromFunction (transExtent (extent arr)) (newElem (index arr))
{-# INLINE [3] traverse #-}

unsafeTraverse arr transExtent newElem
 = fromFunction (transExtent (extent arr)) (newElem (unsafeIndex arr))
{-# INLINE [3] unsafeTraverse #-}


-- | Unstructured traversal over two arrays at once.
traverse2, unsafeTraverse2
        :: forall r1 r2 sh sh' sh'' a b c
        .  ( Source r1 a, Source r2 b
           , Shape sh, Shape sh')
        => Array r1 sh  a               -- ^ First source array.
        -> Array r2 sh' b               -- ^ Second source array.
        -> (sh -> sh' -> sh'')          -- ^ Function to produce the extent of the result.
        -> ((sh -> a) -> (sh' -> b)
                      -> (sh'' -> c))   -- ^ Function to produce elements of the result.
                                        --   It is passed lookup functions to get elements of the
                                        --   source arrays.
        -> Array D sh'' c

traverse2 arrA arrB transExtent newElem
 = fromFunction  (transExtent (extent arrA) (extent arrB))
                 (newElem     (index  arrA) (index  arrB))
{-# INLINE [3] traverse2 #-}

unsafeTraverse2 arrA arrB transExtent newElem
 = fromFunction  (transExtent (extent arrA) (extent arrB))
                 (newElem     (unsafeIndex arrA) (unsafeIndex arrB))
{-# INLINE [3] unsafeTraverse2 #-}


-- | Unstructured traversal over three arrays at once.
traverse3, unsafeTraverse3
        :: forall r1  r2  r3
                  sh1 sh2 sh3 sh4
                  a   b   c   d
        .  ( Source r1 a, Source r2 b, Source r3 c
           , Shape sh1,   Shape sh2,   Shape sh3)
        => Array r1 sh1 a
        -> Array r2 sh2 b
        -> Array r3 sh3 c
        -> (sh1 -> sh2 -> sh3 -> sh4)
        -> (  (sh1 -> a) -> (sh2 -> b)
           -> (sh3 -> c)
           ->  sh4 -> d )
        -> Array D sh4 d

traverse3 arrA arrB arrC transExtent newElem
 = fromFunction (transExtent (extent arrA) (extent arrB) (extent arrC))
                (newElem     (index arrA)  (index arrB)  (index  arrC))
{-# INLINE [3] traverse3 #-}

unsafeTraverse3 arrA arrB arrC transExtent newElem
 = fromFunction (transExtent (extent arrA) (extent arrB) (extent arrC))
                (newElem     (unsafeIndex arrA) (unsafeIndex arrB) (unsafeIndex arrC))
{-# INLINE [3] unsafeTraverse3 #-}


-- | Unstructured traversal over four arrays at once.
traverse4, unsafeTraverse4
        :: forall r1  r2  r3  r4
                  sh1 sh2 sh3 sh4 sh5
                  a   b   c   d   e
        .  ( Source r1 a, Source r2 b, Source r3 c, Source r4 d
           , Shape sh1, Shape sh2, Shape sh3, Shape sh4)
        => Array r1 sh1 a
        -> Array r2 sh2 b
        -> Array r3 sh3 c
        -> Array r4 sh4 d
        -> (sh1 -> sh2 -> sh3 -> sh4 -> sh5 )
        -> (  (sh1 -> a) -> (sh2 -> b)
           -> (sh3 -> c) -> (sh4 -> d)
           ->  sh5 -> e )
        -> Array D sh5 e

traverse4 arrA arrB arrC arrD transExtent newElem
 = fromFunction (transExtent (extent arrA) (extent arrB) (extent arrC) (extent arrD))
                (newElem     (index  arrA) (index  arrB) (index  arrC) (index  arrD))
{-# INLINE [3] traverse4 #-}


unsafeTraverse4 arrA arrB arrC arrD transExtent newElem
 = fromFunction (transExtent (extent arrA) (extent arrB) (extent arrC) (extent arrD))
                (newElem     (unsafeIndex arrA) (unsafeIndex arrB) (unsafeIndex arrC) (unsafeIndex arrD))
{-# INLINE [3] unsafeTraverse4 #-}


