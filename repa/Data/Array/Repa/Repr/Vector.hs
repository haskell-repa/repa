
module Data.Array.Repa.Repr.Vector
        ( V, Array (..)
        , computeVectorS,  computeVectorP
        , fromListVector
        , fromVector
        , toVector)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Eval
import qualified Data.Vector            as V
import qualified Data.Vector.Mutable    as VM
import Control.Monad

-- | Arrays represented as boxed vectors.
--
--   This representation should only be used when your element type doesn't
--   have an `Unbox` instsance. If it does, then use the Unboxed `U`
--   representation will be faster.
data V
data instance Array V sh e
        = AVector !sh !(V.Vector e)
        
deriving instance (Show sh, Show e)
        => Show (Array V sh e)

-- Repr -----------------------------------------------------------------------
-- | Read elements from a boxed vector array.
instance Repr V a where
 linearIndex (AVector _ vec) ix
        = vec V.! ix
 {-# INLINE linearIndex #-}

 unsafeLinearIndex (AVector _ vec) ix
        = vec `V.unsafeIndex` ix
 {-# INLINE unsafeLinearIndex #-}

 extent (AVector sh _)
        = sh
 {-# INLINE extent #-}

 deepSeqArray (AVector sh vec) x 
  = sh `deepSeq` vec `seq` x
 {-# INLINE deepSeqArray #-}


-- Fill -----------------------------------------------------------------------
-- | Filling of boxed vector arrays.
instance Fillable V e where
 data MArr V e 
  = MVec (VM.IOVector e)

 newMArr n
  = liftM MVec (VM.new n)
 {-# INLINE newMArr #-}

 unsafeWriteMArr (MVec v) ix
  = VM.unsafeWrite v ix
 {-# INLINE unsafeWriteMArr #-}

 unsafeFreezeMArr sh (MVec mvec)     
  = do  vec     <- V.unsafeFreeze mvec
        return  $  AVector sh vec
 {-# INLINE unsafeFreezeMArr #-}

 deepSeqMArr !_vec x
  = x
 {-# INLINE deepSeqMArr #-}

 touchMArr _ 
  = return ()
 {-# INLINE touchMArr #-}


-- Conversions ----------------------------------------------------------------
-- | Sequential computation of array elements.
--
--   * This is an alias for `compute` with a more specific type.
--
computeVectorS
        :: Fill r1 V sh e
        => Array r1 sh e -> Array V sh e
computeVectorS   = computeS
{-# INLINE computeVectorS #-}


-- | Parallel computation of array elements.
computeVectorP
        :: (Fill r1 V sh e, Monad m)
        => Array r1 sh e -> m (Array V sh e)
computeVectorP   = computeP
{-# INLINE computeVectorP #-}


-- | O(n). Convert a list to a boxed vector array.
--
--   * This is an alias for `fromList` with a more specific type.
--
fromListVector :: Shape sh => sh -> [a] -> Array V sh a
fromListVector  = fromList
{-# INLINE fromListVector #-}


-- | O(1). Wrap a boxed vector as an array.
fromVector
        :: Shape sh
        => sh -> V.Vector e -> Array V sh e
fromVector sh vec
        = AVector sh vec
{-# INLINE fromVector #-}


-- | O(1). Unpack a boxed vector from an array.
toVector :: Array V sh e -> V.Vector e
toVector (AVector _ vec)
        = vec
{-# INLINE toVector #-}


