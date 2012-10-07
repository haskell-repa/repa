module Data.Array.Repa.Repr.Stream
        ( S
        , Array (..)
        , Distro(..)
        , vstream)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa                  as R
import Data.Array.Repa.Stream           (Distro(..), DistStream(..))
import qualified Data.Array.Repa.Stream as S
-- import qualified Data.Vector.Unboxed    as U
import GHC.Exts


-- | A delayed array defined by a distributed stream.
data S


-- | Stream arrays.
---
--   This contains a lazy cache of the unchained elements, which would be
--   produced if we were to evaluate the whole chain with unchain{P,S}.
--
--   Random access indexing operations can use this to force evaluation 
--   of the chain at this particular point, and re-computing chain prefixes
--   for every element accessed.
--
instance Source S e where
 data Array S sh e
        =  forall r
        .  Source r e
        => AStream
                sh
                (DistStream e)
                (Array r sh e)
                -- A LAZY cache of the unstreamed elements.

 extent (AStream ex _ _)
  = ex
 {-# INLINE extent #-}

 -- Use the cache when retrieving single elements in a random-access manned.
 -- The first time we index into the vector all elements will be computed,
 -- but then successive operations will use the same cache.
 linearIndex (AStream _ _ vec) ix
  = linearIndex vec ix
 {-# INLINE linearIndex #-}

 deepSeqArray (AStream _ _ vec) x
  = vec `seq` x
 {-# INLINE deepSeqArray #-}


-- | Convert an arbitrary vector to a stream.
vstream :: Source r a => Distro -> Vector r a -> Vector S a
vstream distro vec
 = AStream (Z :. len) (S.streamD distro get) vec
 where  len     = I# (distroLength distro)
        get ix  = R.unsafeLinearIndex vec (I# ix)
        {-# INLINE get #-}
{-# INLINE [1] vstream #-}

