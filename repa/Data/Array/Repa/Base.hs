
module Data.Array.Repa.Base
        ( Array
        , Repr (..), (!)
        , deepSeqArrays)
where
import Data.Array.Repa.Shape


-- | Arrays with a representation tag, shape, and element type.
data family Array r sh e


-- | Class of array representations that we can read elements from.
--
class Repr r e where
 -- | Take the extent of an array.
 extent       :: Shape sh => Array r sh e -> sh

 -- | Shape polymorphic indexing
 index, unsafeIndex
        :: Shape sh => Array r sh e -> sh -> e

 {-# INLINE index #-}
 index arr ix           = arr `linearIndex`       toIndex (extent arr) ix

 {-# INLINE unsafeIndex #-}
 unsafeIndex arr ix     = arr `unsafeLinearIndex` toIndex (extent arr) ix

 -- | Linear indexing into underlying representation
 linearIndex, unsafeLinearIndex
        :: Shape sh => Array r sh e -> Int -> e

 {-# INLINE unsafeLinearIndex #-}
 unsafeLinearIndex      = linearIndex

 -- | Ensure an array's data structure is fully evaluated.
 deepSeqArray :: Shape sh => Array r sh e -> b -> b


-- | Alias for `index`
(!) :: (Repr r e, Shape sh) => Array r sh e -> sh -> e
(!) = index


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

