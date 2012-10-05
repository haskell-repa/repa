{-# LANGUAGE UndecidableInstances #-}
module Data.Array.Repa.Repr.Chain
        ( N
        , Array(..)
        , vcache)
where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Chain            as C      
import Data.Array.Repa                  as R
import qualified Data.Vector.Unboxed    as U
import GHC.Exts

-- | A delayed array defined by chain fragments.
--
--      Chains are like streams, except they are logically divided into several
--      fragments when created. When we finally compute them, each fragment
--      can be evaluated in a different thread.
--
--      We also know how many elements will be produced before evaluating it,
--      unlike streams where this is not known. This implies that chains do
--      not support filtering operatons, but means that computed elements can
--      be written directly into the target vector, instead of needing a copying
--      join.
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


vcache :: U.Unbox e => DistChain e -> Vector N e
vcache dchain
 = let  len     = I# (distroLength (distChainDistro dchain))
   in   AChained       (Z :. len) dchain 
         $ fromUnboxed (Z :. len) 
         $ vunchainD dchain


-- Maps ----------------------------------------------------------------------
instance Map N a where
 type TM N   = N

 vmap f (AChained sh dchain arr)
  = AChained sh (C.mapD f dchain) (R.map f arr)
 {-# INLINE vmap #-}



