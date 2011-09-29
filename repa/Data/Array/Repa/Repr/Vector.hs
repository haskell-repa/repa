
module Data.Array.Repa.Repr.Vector
        ( V, Array (..)
        , fromVector, toVector,  forceVector
        , fromList,   toList)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Eval.Fill
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
-- | Filling of unboxed vector arrays.
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


-- Load -----------------------------------------------------------------------
-- | no-op.
instance Shape sh => Load V V sh e where
 {-# INLINE load #-}
 load arr = arr


-- Conversions ----------------------------------------------------------------
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


-- | Force an array to a boxed vector.
--
--   If the source array was in delayed form then this invokes parallel computation.
--
forceVector
        :: (Load r1 V sh e)
        => Array r1 sh e -> Array V sh e
{-# INLINE forceVector #-}
forceVector = load


-- | O(n). Convert a list to a boxed vector.
fromList :: sh -> [a] -> Array V sh a
{-# INLINE fromList #-}
fromList sh xs
        = AVector sh (V.fromList xs)


-- | O(n). Convert an array to a list.
toList  :: (Shape sh, Repr r a, Load D V sh a)
        => Array r sh a -> [a]
{-# INLINE toList #-}
toList arr
 = case load (delay arr) of
         AVector _ vec -> V.toList vec


