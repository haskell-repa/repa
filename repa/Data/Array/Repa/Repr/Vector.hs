
module Data.Array.Repa.Repr.Vector
        ( V, Array (..)
        , computeVector,  fromVector, toVector
        , fromListVector, toListVector)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.Delayed
import qualified Data.Vector            as V
import qualified Data.Vector.Mutable    as VM
import Control.Monad

-- | Arrays represented as boxed vectors.
data V
data instance Array V sh e
        = AVector sh !(V.Vector e)
        
deriving instance (Show sh, Show e)
        => Show (Array V sh e)

-- Repr -----------------------------------------------------------------------
-- | Use elements from an unboxed vector array.
instance Repr V a where
 {-# INLINE linearIndex #-}
 linearIndex (AVector _ vec) ix
        = vec V.! ix

 {-# INLINE unsafeLinearIndex #-}
 unsafeLinearIndex (AVector _ vec) ix
        = vec `V.unsafeIndex` ix

 {-# INLINE extent #-}
 extent (AVector sh _)
        = sh

 {-# INLINE deepSeqArray #-}
 deepSeqArray (AVector sh vec) x 
  = sh `deepSeq` vec `seq` x


-- Fill -----------------------------------------------------------------------
-- | Filling of boxed vector arrays.
instance Fillable V e where
 data MArr V e 
  = MVec (VM.IOVector e)

 {-# INLINE newMArr #-}
 newMArr n
  = liftM MVec (VM.new n)

 {-# INLINE unsafeWriteMArr #-}
 unsafeWriteMArr (MVec v) ix
  = VM.unsafeWrite v ix

 {-# INLINE unsafeFreezeMArr #-}
 unsafeFreezeMArr sh (MVec mvec)     
  = do  vec     <- V.unsafeFreeze mvec
        return  $  AVector sh vec


-- Conversions ----------------------------------------------------------------
-- | Compute an array to a boxed vector.
--
--   * This is just a wrapper for `compute`, with a more specific type.
--
computeVector
        :: Fill r1 V sh e
        => Array r1 sh e -> Array V sh e
{-# INLINE computeVector #-}
computeVector = compute


-- | O(1). Wrap a boxed vector as an array
fromVector
        :: Shape sh
        => sh -> V.Vector e -> Array V sh e
{-# INLINE fromVector #-}
fromVector sh vec
        = AVector sh vec


-- | O(1). Unpack a vector from an array.
toVector :: Array V sh e -> V.Vector e
{-# INLINE toVector #-}
toVector (AVector _ vec)
        = vec


-- | O(n). Convert a list to a boxed vector.
fromListVector :: sh -> [a] -> Array V sh a
{-# INLINE fromListVector #-}
fromListVector sh xs
        = AVector sh (V.fromList xs)


-- | O(n). Convert an array to a list.
toListVector :: Shape sh
        => Array V sh a -> [a]
{-# INLINE toListVector #-}
toListVector (AVector _ vec) 
        = V.toList vec

