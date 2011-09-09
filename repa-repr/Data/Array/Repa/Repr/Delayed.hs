
module Data.Array.Repa.Repr.Delayed
        ( D, Array(..)
        , fromFunction, toFunction
        , delay
        , copy)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base


-- | Delayed arrays are represented as functions from the index to element value.
data D
data instance Array D sh e
        = Delayed  sh (sh -> e)


instance Repr D a where
 {-# INLINE index #-}
 index  (Delayed _ f) ix
        = f ix

 {-# INLINE extent #-}
 extent (Delayed sh _)
        = sh

 {-# INLINE deepSeqArray #-}
 deepSeqArray (Delayed sh f) y
        = sh `deepSeq` f `seq` y

instance Repr r1 e => Load r1 D e where
 {-# INLINE load #-}
 load arr = Delayed (extent arr) (\ix -> index arr ix)


-- | O(1). Wrap a function as a delayed array.
fromFunction :: sh -> (sh -> a) -> Array D sh a
{-# INLINE fromFunction #-}
fromFunction sh f = Delayed sh f


-- | O(1). Unpack an array to a function,
--   which produces a function to retrieve an arbitrary element.
toFunction 
        :: (Shape sh, Repr r1 a)
        => Array r1 sh a -> (sh, sh -> a)
{-# INLINE toFunction #-}
toFunction arr
 = case load arr of
        Delayed sh f      -> (sh, f)


-- | O(1). Delay an array.
delay   :: (Shape sh, Repr r e)
        => Array r sh e -> Array D sh e
{-# INLINE delay #-}
delay = load


-- | Copy an array by delaying it then loading to the new representation.
copy    :: (Shape sh, Repr r1 e, Load D r2 e)
        => Array r1 sh e -> Array r2 sh e
{-# INLINE copy #-}
copy    = load . delay
