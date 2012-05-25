
module Data.Array.Repa.Base
        ( Source (..), (!), toList
        , deepSeqArrays)
where
import Data.Array.Repa.Shape


-- Source -----------------------------------------------------------------------
-- | Class of array representations that we can read elements from.
class Source r e where
 -- Arrays with a representation tag, shape, and element type.
 --   Use one of the type tags like `D`, `U` and so on for @r@, 
 --   one of `DIM1`, `DIM2` ... for @sh@.
 data Array r sh e

 -- | O(1). Take the extent (size) of an array.
 extent :: Shape sh => Array r sh e -> sh

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
 deepSeqArray 
        :: Shape sh =>Array r sh e -> b -> b


-- | O(1). Alias for `index`
(!) :: Shape sh => Source r e => Array r sh e -> sh -> e
(!) = index


-- | O(n). Convert an array to a list.
toList  :: Shape sh => Source r e
        => Array r sh e -> [e]
{-# INLINE toList #-}
toList arr 
 = go 0 
 where  len     = size (extent arr)
        go ix
         | ix == len    = []
         | otherwise    = unsafeLinearIndex arr ix : go (ix + 1)


-- | Apply `deepSeqArray` to up to four arrays. 
---
--   NOTE: this shouldn't be needed anymore, as we've made all the shape fields strict.
--      
--   The implementation of this function has been hand-unwound to work for up to
--   four arrays. Putting more in the list yields `error`.
-- 
--   For functions that are /not/ marked as INLINE, you should apply `deepSeqArrays`
--   to argument arrays before using them in a @compute@ or @copy@ expression.
--   For example:
--
-- @  processArrays 
--     :: Monad m 
--     => Array U DIM2 Int -> Array U DIM2 Int 
--     -> m (Array U DIM2 Int)
--  processArrays arr1 arr2
--   = [arr1, arr2] \`deepSeqArrays\` 
--     do arr3 <- computeP $ map f arr1
--        arr4 <- computeP $ zipWith g arr3 arr2
--        return arr4
--  @
--
--  Applying `deepSeqArrays` tells the GHC simplifier that it's ok to unbox 
--  size fields and the pointers to the underlying array data at the start
--  of the function. Without this, they may be unboxed repeatedly when
--  computing elements in the result arrays, which will make your program slow.
--
--  If you INLINE @processArrays@ into the function that computes @arr1@ and @arr2@,
--  then you don't need to apply `deepSeqArrays`. This is because a pointer
--  to the underlying data will be passed directly to the consumers and never boxed.
--
--  If you're not sure, then just follow the example code above.
--   
deepSeqArrays 
        :: Shape sh => Source r e
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

 

