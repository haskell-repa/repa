
module Data.Array.Repa.Vector.Operators.Traverse
        ( traverse
        , traverse2)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Operators.Bulk
import Data.Array.Repa.Vector.Repr.Delayed


-- | Unstructured traversal.
traverse
        :: forall r sh1 sh2 a b
        .  (Bulk r a, Shape sh1, Shape sh2)
        => Array r sh1 a                -- ^ Source array.
        -> (sh1  -> sh2)                -- ^ Function to produce the extent of the result.
        -> ((sh1 -> a) -> sh2 -> b)     -- ^ Function to produce elements of the result.
                                        --   It is passed a lookup function to get elements of the source.
        -> Array D sh2 b

traverse arr transExtent newElem
 = fromFunction (transExtent (extent arr)) (newElem (index arr))
{-# INLINE [4] traverse #-}


-- | Unstructured traversal over two arrays at once.
traverse2
        :: forall r1 r2 sh1 sh2 sh3 a b c
        .  ( Bulk r1 a, Bulk r2 b
           , Shape sh1, Shape sh2, Shape sh3)
        => Array r1 sh1 a               -- ^ First source array.
        -> Array r2 sh2 b               -- ^ Second source array.
        -> (sh1 -> sh2 -> sh3)          -- ^ Function to produce the extent of the result.
        -> ((sh1 -> a) -> (sh2 -> b)
                       -> (sh3 -> c))   -- ^ Function to produce elements of the result.
                                        --   It is passed lookup functions to get elements of the
                                        --   source arrays.
        -> Array D sh3 c

traverse2 arrA arrB transExtent newElem
 = fromFunction  (transExtent (extent arrA) (extent arrB))
                 (newElem     (index  arrA) (index  arrB))
{-# INLINE [4] traverse2 #-}


