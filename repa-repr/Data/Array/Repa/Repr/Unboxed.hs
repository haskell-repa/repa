
module Data.Array.Repa.Repr.Unboxed
        ( U, Array (..)
        , fromUnboxed, toUnboxed
        , force)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Repr.Delayed
import qualified Data.Vector.Unboxed      as U


-- | Unboxed arrays are represented as unboxed vectors.
data U
data instance Array U sh e
        = AUnboxed sh !(U.Vector e)
        
deriving instance (Show sh, Show e, U.Unbox e)
        => Show (Array U sh e)


instance U.Unbox a => Repr U a where
 {-# INLINE index #-}
 index  (AUnboxed sh  vec) ix 
        = vec U.! toIndex sh ix

 {-# INLINE unsafeIndex #-}
 unsafeIndex (AUnboxed sh vec) ix
        = vec `U.unsafeIndex` toIndex sh ix

 {-# INLINE extent #-}
 extent (AUnboxed sh _)
        = sh

 {-# INLINE deepSeqArray #-}
 deepSeqArray (AUnboxed sh vec) x 
  = sh `deepSeq` vec `seq` x

instance U.Unbox e => Load U U e where
 {-# INLINE load #-}
 load arr = arr


instance U.Unbox e => Load D U e where
 {-# INLINE load #-}
 load (Delayed sh getElem)
        = AUnboxed sh
        $ U.generate (size sh) (getElem . fromIndex sh)


-- | O(1). Wrap an unboxed vector as an array.
fromUnboxed
        :: (Shape sh, U.Unbox e)
        => sh -> U.Vector e -> Array U sh e
{-# INLINE fromUnboxed #-}
fromUnboxed sh vec = AUnboxed sh vec


-- | Convert an array to a flat unboxed vector.
toUnboxed
        :: (Shape sh, Load r1 U e, U.Unbox e) 
        => Array r1 sh e -> U.Vector e
toUnboxed arr
 = case load arr of
        AUnboxed _ vec  -> vec


-- | Force an array to an unboxed vector.
force   :: (Shape sh, Load r1 U e)
        => Array r1 sh e -> Array U sh e
{-# INLINE force #-}
force = load


