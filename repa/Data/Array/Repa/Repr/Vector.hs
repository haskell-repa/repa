
module Data.Array.Repa.Repr.Vector
        ( V, Array (..)
        , computeVector, fromListVector
        , fromVector, toVector)
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
-- | Read elements from a boxed vector array.
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
-- | Compute array elements in parallel.
--
--   * This is an alias for `compute` with a more specific type.
--
computeVector
        :: Fill r1 V sh e
        => Array r1 sh e -> Array V sh e
{-# INLINE computeVector #-}
computeVector   = compute


-- | O(n). Convert a list to a boxed vector array.
--
--   * This is an alias for `fromList` with a more specific type.
--
fromListVector :: Shape sh => sh -> [a] -> Array V sh a
{-# INLINE fromListVector #-}
fromListVector  = fromList


-- | O(1). Wrap a boxed vector as an array.
fromVector
        :: Shape sh
        => sh -> V.Vector e -> Array V sh e
{-# INLINE fromVector #-}
fromVector sh vec
        = AVector sh vec


-- | O(1). Unpack a boxed vector from an array.
toVector :: Array V sh e -> V.Vector e
{-# INLINE toVector #-}
toVector (AVector _ vec)
        = vec


