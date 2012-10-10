{-# LANGUAGE UndecidableInstances #-}
module Data.Array.Repa.Repr.Chain
        ( N
        , Array (..)
        , Distro(..)
        , vchain
        , vchainWith
        , vcacheChain)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa                  as R
import Data.Array.Repa.Chain            (Distro(..), DistChain(..))
import qualified Data.Array.Repa.Chain  as C      
import qualified Data.Vector.Unboxed    as U
import GHC.Exts

-- | A delayed array defined by a distributed chain.
--
--      A chain is like a stream, except that we know how many elements will
--      be produced by each thread before evaluating them. This information is 
--      collected into a `Distro`.
--
--      Because we know the distribution ahead of time, when we compute the 
--      vector we can write the result elements directly into the target
--      buffer without requiring a copying join.
--
--      The tradeoff is that chains do not support filtering operations, 
--      because we won't know how many elements will be produced by each 
--      thread before computing them.
--
data N


-- | Chained arrays.
instance Source N e where
 data Array N sh e
        =  forall r
        .  Source r e
        => AChained
                sh
                (DistChain e)
                (Array r sh e)
                -- A LAZY cache of the unchained elements.

 extent (AChained ex _ _)
  = ex
 {-# INLINE extent #-}

 -- Use the cache when retrieving single elements in a random-access manned.
 -- The first time we index into the vector all elements will be computed,
 -- but then successive operations will use the same cache.
 linearIndex (AChained _ _ vec) ix
  = linearIndex vec ix
 {-# INLINE linearIndex #-}

 deepSeqArray (AChained _ _ vec) x
  = vec `seq` x
 {-# INLINE deepSeqArray #-}


-- | Convert an arbitrary vector to a chain.
--
--   The vector is divided evenly among the threads of the global Repa gang.
vchain :: Source r a => Vector r a -> Vector N a
vchain vec = vchainWith (balanced (vlength vec)) vec
{-# INLINE [1] vchain #-}


-- | Convert an arbitrary vector to a stream, using a custom `Distro`.
--
--   The `C.Distro` length must match the vector length, else undefined.
vchainWith :: Source r a => C.Distro -> Vector r a -> Vector N a
vchainWith distro vec
 = AChained (Z :. len) (C.chainD distro get) vec
 where  len     = I# (distroLength distro)
        get ix  = R.unsafeLinearIndex vec (I# ix)
        {-# INLINE get #-}
{-# INLINE [1] vchainWith #-}


-- | Build a chained vector from an underlying `DistChain`.
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
vcacheChain :: U.Unbox e => DistChain e -> Vector N e
vcacheChain dchain
 = let  len     = I# (distroLength (distChainDistro dchain))
   in   AChained       (Z :. len) dchain 
         $ fromUnboxed (Z :. len) 
         $ C.unchainUnboxedD dchain     -- TODO: use parallel evaluation


