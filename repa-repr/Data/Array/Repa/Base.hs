
module Data.Array.Repa.Base
        ( Array
        , Repr (..)
        , Load (..)
        , deepSeqArrays)
where
import Data.Array.Repa.Shape

-- | Array type with a shape, representation tag, and element type
data family Array r sh e


-- | Operators that array representations implement differently. 
class Repr r e where
 index        :: Shape sh => Array r sh e -> sh -> e

 unsafeIndex  :: Shape sh => Array r sh e -> sh -> e
 unsafeIndex = index
 
 extent       :: Shape sh => Array r sh e -> sh

 deepSeqArray :: Shape sh => Array r sh e -> b -> b


-- | Load array data between representations.
--
--   * Loading between arrays of the same representation is a no-op.
--
--   * Loading a delayed array to an array-like manifest representation invokes
--     parallel computation. (unboxed vectors are array-like, but lists are not)
--
--   * Loading between manifest representations can be constant time or require a
--     parallel copy, depending on whether the two representations can be easily
--     converted.
--
class (Repr r1 e, Repr r2 e) => Load r1 r2 e where
 load :: Shape sh => Array r1 sh e  -> Array r2 sh e



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

