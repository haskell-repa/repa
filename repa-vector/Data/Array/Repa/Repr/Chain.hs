{-# LANGUAGE UndecidableInstances #-}
module Data.Array.Repa.Repr.Chain
        ( N
        , Array (..)
        , Distro(..)
        , vchain
        , vcache)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa                  as R
import Data.Array.Repa.Chain            (Distro(..), DistChain(..))
import qualified Data.Array.Repa.Chain  as C      
import qualified Data.Vector.Unboxed    as U
import GHC.Exts

-- | A delayed array defined by chain fragments.
--
--      A chain is like a stream, except that we know how many elements will
--      be produced on each node before evaluating it. This information is 
--      collected into a `Distro`.
--
--      The fact that we must know the distribution of the result ahead of time, 
--      means that chains do not support filtering operations. However, when 
--      evaluated in parallel we can write the computed elements directly to
--      the target buffer, without needing a copying join to collect the results
--      from each node.
--
data N


-- | Chained arrays.
---
--   This contains a lazy cache of the unchained elements, which would be
--   produced if we were to evaluate the whole chain with unchain{P,S}.
--
--   Random access indexing operations can use this to force evaluation 
--   of the chain at this particular point, and re-computing chain prefixes
--   for every element accessed.
--
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
vchain :: Source r a => Distro -> Vector r a -> Vector N a
vchain distro vec
 = AChained (Z :. len) (C.chainD distro get) vec
 where  len     = I# (distroLength distro)
        get ix  = R.unsafeLinearIndex vec (I# ix)
        {-# INLINE get #-}
{-# INLINE [1] vchain #-}


-- | Build a vector from a `DistChain`.
--
--   The `Vector` contains a suspended cache of evaluated elements, 
--   which we will use for random-access indexing.
--
vcache :: U.Unbox e => DistChain e -> Vector N e
vcache dchain
 = let  len     = I# (distroLength (distChainDistro dchain))
   in   AChained       (Z :. len) dchain 
         $ fromUnboxed (Z :. len) 
         $ C.vunchainD dchain             -- TODO: use parallel evaluation


-- Maps ----------------------------------------------------------------------
instance Map N a where
 type TM N   = N

 vmap f (AChained sh dchain arr)
  = AChained sh (C.mapD f dchain) (R.map f arr)
 {-# INLINE vmap #-}


-- Unboxed/Chained ------------------------------------------------------------
instance Zip N N a b where
 type TZ N N            = N
 vzip !(AChained sh1 dchain1 vec1)
      !(AChained _   dchain2 vec2)
  =     AChained sh1 (C.zipWithD (,) dchain1 dchain2) (R.zipWith (,) vec1 vec2)


instance U.Unbox a => Zip U N a b where
 type TZ U N            = N
 vzip !arr1 !arr2@(AChained _ dchain _)
        = vzip (vchain (distChainDistro dchain) arr1) arr2
 {-# INLINE [1] vzip #-}


instance U.Unbox b => Zip N U a b where
 type TZ N U            = N
 vzip !arr1@(AChained _ dchain _) !arr2       
        = vzip arr1 (vchain (distChainDistro dchain) arr2)
 {-# INLINE [1] vzip #-}

