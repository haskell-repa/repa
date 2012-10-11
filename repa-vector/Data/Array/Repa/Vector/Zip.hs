
module Data.Array.Repa.Vector.Zip
        ( Zip (..)
        , vzipWith)
where
import Data.Array.Repa.Repr.Stream
import Data.Array.Repa.Repr.Chain
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Map
import Data.Array.Repa.Chain.Map        as C
import Data.Array.Repa                  as R
import qualified Data.Vector.Unboxed    as U
import GHC.Exts

-- Zip ------------------------------------------------------------------------
-- | Vector zip uses the least general representation for the result.
--
--   For example, zipping two Delayed (@D@) arrays produces a delayed array,
--   but zipping a Delayed (@D@) and a Chained (@N@) array must produce
--   a chained array.
--
--   We don't support Chained+Chained or Streamed+Streamed zips becauese
--   the distributions of the two vectors may not be the same. You'll need to 
--   compute one of the vectors to redistribute it.
--
class Zip r1 r2 a b where
 type ZipR r1 r2
 vzip   :: Vector r1 a 
        -> Vector r2 b
        -> Vector (ZipR r1 r2) (a, b)


-- Unboxed/Unboxed --------------------
instance (U.Unbox a, U.Unbox b) 
       => Zip U U a b where
 type ZipR U U  = D
 vzip !arr1 !arr2
        = vzip (delay arr1) (delay arr2)
 {-# INLINE [1] vzip #-}


-- Unboxed/Delayed
instance U.Unbox a
      => Zip U D a b where
 type ZipR U D  = D
 vzip !arr1 !arr2
        = vzip (delay arr1) arr2
 {-# INLINE [1] vzip #-}


-- Unboxed/Chained
instance U.Unbox a => Zip U N a b where
 type ZipR U N          = N
 vzip !arr1 (AChain sh dchain vec)
        = AChain sh
                (C.mapIxD (\i x -> (unsafeIndex arr1 (Z :. (I# i)), x))
                          dchain)
                (R.zipWith   (,) arr1 vec)
 {-# INLINE [1] vzip #-}


-- Delayed/Unboxed --------------------
instance U.Unbox b
      => Zip D U a b where
 type ZipR D U    = D
 vzip !arr1 !arr2
        = vzip arr1 (delay arr2)
 {-# INLINE [1] vzip #-}

-- Delayed/Delayed
instance Zip D D a b where
 type ZipR D D    = D
 vzip !arr1 !arr2
  = fromFunction (extent arr1) get
  where get ix = ( arr1 `unsafeIndex` ix
                 , arr2 `unsafeIndex` ix)
        {-# INLINE get #-}
 {-# INLINE [1] vzip #-}

-- Delayed/Chained
instance Zip D N a b where
 type ZipR D N  = N
 vzip !arr1 (AChain sh dchain vec)
        = AChain sh
                (C.mapIxD (\i x -> (unsafeIndex arr1 (Z :. (I# i)), x))
                          dchain)
                (R.zipWith (,) arr1 vec)
 {-# INLINE [1] vzip #-}


-- Chained/Unboxed --------------------
instance U.Unbox b => Zip N U a b where
 type ZipR N U  = N
 vzip (AChain sh dchain vec) !arr2
        = AChain sh
                (C.mapIxD (\i x -> (x, unsafeIndex arr2 (Z :. (I# i))))
                          dchain)
                (R.zipWith (,) vec arr2)
 {-# INLINE [1] vzip #-}


-- Chained/Delayed
instance Zip N D a b where
 type ZipR N D  = N
 vzip (AChain sh dchain vec) !arr2
        = AChain sh
                (C.mapIxD (\i x -> (x, unsafeIndex arr2 (Z :. (I# i))))
                          dchain)
                (R.zipWith  (,) vec arr2)
 {-# INLINE [1] vzip #-}


-- ZipWith --------------------------------------------------------------------
-- TODO: we should probably make this primitive instead of zip.
--       that way the type constraints will be simpler.
-- | Combine two vectors with the given function)
vzipWith :: ( Map (ZipR r1 r2) (a, b)
           , Zip r1 r2 a b)
        => (a -> b -> c) -> Vector r1 a -> Vector r2 b 
        -> Vector (MapR (ZipR r1 r2)) c
vzipWith f vec1 vec2
        = vmap (uncurry f) $ vzip vec1 vec2

