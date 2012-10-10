
module Data.Array.Repa.Vector.Zip
        (Zip (..))
where
import Data.Array.Repa.Repr.Chain
import Data.Array.Repa.Chain
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Chain.Map        as C
import Data.Array.Repa                  as R
import qualified Data.Vector.Unboxed    as U


-- Zip ------------------------------------------------------------------------
-- | Vector zip uses the least general representation for the result.
--
--   For example, zipping two Delayed (@D@) arrays produces a delayed array,
--   but zipping a Delayed (@D@) and a Chained (@N@) array must produce
--   a chained array.
class Zip r1 r2 a b where
 type ZipR r1 r2
 vzip   :: Vector r1 a 
        -> Vector r2 b
        -> Vector (ZipR r1 r2) (a, b)


-- Unboxed/Unboxed --------------------
instance (U.Unbox a, U.Unbox b) 
       => Zip U U a b where
 type ZipR U U            = D
 vzip !arr1 !arr2       = vzip (delay arr1) (delay arr2)
 {-# INLINE [4] vzip #-}


-- Unboxed/Delayed
instance U.Unbox a
      => Zip U D a b where
 type ZipR U D          = D
 vzip !arr1 !arr2       = vzip (delay arr1) arr2
 {-# INLINE [4] vzip #-}


-- Unboxed/Chained
instance U.Unbox a => Zip U N a b where
 type ZipR U N          = N
 vzip !arr1 !arr2@(AChained _ dchain _)
        = vzip (vchainWith (distChainDistro dchain) arr1) arr2
 {-# INLINE [1] vzip #-}


-- Delayed/Unboxed --------------------
instance U.Unbox b
      => Zip D U a b where
 type ZipR D U    = D
 vzip !arr1 !arr2       = vzip arr1 (delay arr2)
 {-# INLINE [4] vzip #-}

-- Delayed/Delayed
instance Zip D D a b where
 type ZipR D D    = D
 vzip !arr1 !arr2
  = fromFunction (extent arr1) get
  where get ix = ( arr1 `unsafeIndex` ix
                 , arr2 `unsafeIndex` ix)
        {-# INLINE get #-}
 {-# INLINE [4] vzip #-}

-- Delayed/Chained
instance Zip D N a b where
 type ZipR D N  = N
 vzip !arr1 !arr2@(AChained _ dchain _)
        = vzip (vchainWith (distChainDistro dchain) arr1) arr2


-- Chained/Unboxed --------------------
instance U.Unbox b => Zip N U a b where
 type ZipR N U          = N
 vzip !arr1@(AChained _ dchain _) !arr2       
        = vzip arr1 (vchainWith (distChainDistro dchain) arr2)
 {-# INLINE [1] vzip #-}


-- Chained/Delayed
instance U.Unbox b => Zip N D a b where
 type ZipR N D          = N
 vzip !arr1@(AChained _ dchain _) !arr2       
        = vzip arr1 (vchainWith (distChainDistro dchain) arr2)
 {-# INLINE [1] vzip #-}


-- Chained/Chained
instance Zip N N a b where
 type ZipR N N          = N
 vzip !(AChained sh1 dchain1 vec1)
      !(AChained _   dchain2 vec2)
  =     AChained sh1 (C.zipWithD (,) dchain1 dchain2) (R.zipWith (,) vec1 vec2)

