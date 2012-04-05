
module Data.Array.Repa.Base
        ( Array
        , Repr (..), (!), toList
        , deepSeqArrays)
where
import Data.Array.Repa.Shape

-- | Arrays with a representation tag, shape, and element type.
--   Use one of the type tags like `D`, `U` and so on for @r@, 
--   one of `DIM1`, `DIM2` ... for @sh@.
data family Array r sh e


-- Repr -----------------------------------------------------------------------
-- | Class of array representations that we can read elements from.
--
class Repr r e where
 -- | O(1). Take the extent of an array.
 extent       :: Shape sh => Array r sh e -> sh

 -- | O(1). Shape polymorphic indexing.
 index, unsafeIndex
        :: Shape sh => Array r sh e -> sh -> e

 {-# INLINE index #-}
 index arr ix           = arr `linearIndex`       toIndex (extent arr) ix

 {-# INLINE unsafeIndex #-}
 unsafeIndex arr ix     = arr `unsafeLinearIndex` toIndex (extent arr) ix

 -- | O(1). Linear indexing into underlying, row-major, array representation.
 linearIndex, unsafeLinearIndex
        :: Shape sh => Array r sh e -> Int -> e

 {-# INLINE unsafeLinearIndex #-}
 unsafeLinearIndex      = linearIndex

 -- | Ensure an array's data structure is fully evaluated.
 deepSeqArray :: Shape sh => Array r sh e -> b -> b


-- | O(1). Alias for `index`
(!) :: (Repr r e, Shape sh) => Array r sh e -> sh -> e
(!) = index


-- | O(n). Convert an array to a list.
toList  :: (Shape sh, Repr r e)
        => Array r sh e -> [e]
{-# INLINE toList #-}
toList arr 
 = go 0 
 where  len     = size (extent arr)
        go ix
         | ix == len    = []
         | otherwise    = unsafeLinearIndex arr ix : go (ix + 1)


-- | Apply `deepSeqArray` to up to four arrays. 
--
--   The implementation of this function has been hand-unwound to work for up to
--   four arrays. Putting more in the list yields `error`.
-- 
deepSeqArrays 
        :: (Shape sh, Repr r e)
        => [Array r sh e] -> b -> b
{-# INLINE deepSeqArrays #-}
deepSeqArrays arrs x
 = case arrs of
        []              -> x

        [a1]
         -> a1 `deepSeqArray` x

        [a1, a2]
         -> a1 `deepSeqArray` a2 `deepSeqArray` x

        [a1, a2, a3]
         -> a1 `deepSeqArray` a2 `deepSeqArray` a3 `deepSeqArray` x

        [a1, a2, a3, a4]
         -> a1 `deepSeqArray` a2 `deepSeqArray` a3 `deepSeqArray` a4 `deepSeqArray` x

        _ -> error "deepSeqArrays: only works for up to four arrays"

 

