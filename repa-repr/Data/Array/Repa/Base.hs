
module Data.Array.Repa.Base
        ( Array
        , Repr (..)
        , Load (..), Load2(..)
        , deepSeqArrays)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Index

-- | Arrays with a representation tag, shape, and element type.
data family Array r sh e


-- | Class of supported array representations.
--
--   These operators are used to read the physical data,
--   or to compute it for the delayed and cursored representations.
--
class Repr r e where
 extent       :: Shape sh => Array r sh e -> sh

 index        :: Shape sh => Array r sh e -> sh -> e

 unsafeIndex  :: Shape sh => Array r sh e -> sh -> e
 unsafeIndex = index

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
class Load r1 r2 e where
 load :: Shape sh => Array r1 sh e  -> Array r2 sh e

-- | Array loading specialised to rank-2 arrays.
--
--  * Instances should perform cache-friendly blockwise filling.
-- 
--  * This is instantiated to arrays with 5 partitions, so that you can use 
--    separate element functions for the border and internal regions.
class Load2 r1 r2 e where
 load2 :: Array r1 DIM2 e -> Array r2 DIM2 e


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

