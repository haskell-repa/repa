
module Data.Array.Repa.Vector.Operators.Combine
        ( combine2
        , combines2)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Unboxed
import Data.Array.Repa.Vector.Operators.Bulk
import Data.Array.Repa.Vector.Segd              (Segd)
import qualified Data.Array.Repa.Vector.Segd    as Segd
import qualified Data.Array.Repa.Flow.Seq       as F
import qualified Data.Vector.Unboxed            as U
import GHC.Exts

-- | Combine two arrays with a flags vector.
--
--   TODO: make this parallel and fusable.
--
combine2 
        :: (U.Unbox a, Elt a)
        => Vector U Bool        -- ^ Flags vector.
        -> Vector U a           -- ^ Elements of @A@ vector.
        -> Vector U a           -- ^ Elements of @B@ vector.
        -> Vector U a

combine2 fs xs ys
        = AUnboxed 
                (extent fs)
                (F.unflow $ F.combine2 
                        (flowUnboxed fs)
                        (flowUnboxed xs) 
                        (flowUnboxed ys))
{-# INLINE [4] combine2 #-}


-- | Segmented combine. Like `combine2`, except that the flags select
--   entire segments of each data stream, instead of selecting one element
--   at a time.
--
-- @
-- combines2 
--      [F, F, T, F, T, T]
--      [2,1,3] [10,20,30,40,50,60]
--      [1,2,3] [11,22,33,44,55,66]
--  = [10,20,30,11,40,50,60,22,33,44,55,66]
-- @
--
--   This says take two elements from the first stream, then another one element 
--   from the first stream, then one element from the second stream, then three
--   elements from the first stream...
--
--   TODO: make this parallel and fusable.
--
combines2
        :: (U.Unbox a, Elt a)
        => Vector U Bool        -- ^ Flags vector.
        -> Segd                 -- ^ Segment descriptor of 'A' vector.
        -> Vector U a           -- ^ Elements of @A@ vector.
        -> Segd                 -- ^ Segment descriptor of 'B' vector.
        -> Vector U a           -- ^ Elements of @A@ vector.
        -> Vector U a

combines2 fs segdA elemsA segdB elemsB
        = AUnboxed
                (extent fs)
                (F.unflow $ F.combines2
                        (flowUnboxed fs)
                        (flowUnboxed $ Segd.lengths segdA)
                        (flowUnboxed elemsA)
                        (flowUnboxed $ Segd.lengths segdB)
                        (flowUnboxed elemsB))
{-# INLINE [4] combines2 #-}


flowUnboxed arr
 = F.flow get len
 where  !vec      = toUnboxed arr
        get ix    = (U.!) vec (I# ix)
        !(I# len) = U.length vec
{-# INLINE [4] flowUnboxed #-}
