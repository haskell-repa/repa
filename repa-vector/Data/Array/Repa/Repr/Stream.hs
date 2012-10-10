module Data.Array.Repa.Repr.Stream
        ( S
        , Array (..)
        , Distro(..)
        , vstream
        , vstreamWith
        , vstreamOfChain
        , vcacheStream)
where
import Data.Array.Repa.Repr.Chain
import Data.Array.Repa.Vector.Base
import Data.Array.Repa                  as R
import Data.Array.Repa.Stream           (DistStream(..))
import qualified Data.Array.Repa.Stream as S
import qualified Data.Vector.Unboxed    as U
import GHC.Exts


-- | A delayed array defined by a distributed stream.
--
--   Each thread of the gang has its own local stream fragment.
--   Computing the vector evaluates all stream fragments in parallel, 
--   then the results from each thread are combined with a copying join.
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


-- | Convert an arbitrary vector to a stream, using a balanced `Distro`.
--
--   The vector is divided evenly among the threads of the global Repa gang.
vstream :: Source r a => Vector r a -> Vector S a
vstream vec = vstreamWith (balanced (vlength vec)) vec
{-# INLINE [1] vstream #-}


-- | Convert an arbitrary vector to a stream, using a custom `Distro`.
--
--    The `S.Distro` length must match the vector length, else undefined.
vstreamWith :: Source r a => S.Distro -> Vector r a -> Vector S a
vstreamWith distro vec
 = AStream (Z :. len) (S.streamD distro get) vec
 where  len     = I# (distroLength distro)
        get ix  = R.unsafeLinearIndex vec (I# ix)
        {-# INLINE get #-}
{-# INLINE [1] vstreamWith #-}


-- | Convert a chained vector to a streamed vector.
-- 
--   The conversion itself is cheap, but evaluating the result will then 
--   require a copying join to collect the results from each thread.
vstreamOfChain :: Vector N a -> Vector S a
vstreamOfChain (AChain sh dchain vec)
        = AStream sh (S.streamOfChainD dchain) vec
{-# INLINE [1] vstreamOfChain #-}


-- | Build a streamed vector from an underlying `DistStream`.
--
--   The result `Vector` contains a suspended cache of evaluated elements, 
--   which can be used for random-access indexing.
--
--   The first time the vector is indexed into, the whole lot is evaluated,
--   and the cache is reused for subsequent indexing operations.
--
--   Using vector-specific functions like `vmap` and `vindexed` does not 
--   force evaluation of the cache, as these consume the vector linearly
--   instead of in a random-access manner.
--
vcacheStream :: U.Unbox e => DistStream e -> Vector S e
vcacheStream dstream
 = let  vec     = S.unstreamUnboxedD dstream
        len     = U.length vec
   in   AStream        (Z :. len) dstream
         $ fromUnboxed (Z :. len) 
         $ vec                          -- TODO: use parallel evaluation
{-# INLINE [1] vcacheStream #-}
