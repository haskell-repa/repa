
module Data.Array.Repa.Repr.Unboxed
        ( U, U.Unbox, Array (..)
        , computeUnboxedS, computeUnboxedP
        , fromListUnboxed
        , fromUnboxed, toUnboxed
        
        , zip,   zip3,   zip4,   zip5,   zip6
        , unzip, unzip3, unzip4, unzip5, unzip6)
where
import Data.Array.Repa.Shape            as R
import Data.Array.Repa.Base             as R
import Data.Array.Repa.Eval             as R
import Data.Array.Repa.Repr.Delayed     as R
import qualified Data.Vector.Unboxed              as U
import qualified Data.Vector.Unboxed.Mutable      as UM
import Control.Monad
import Prelude hiding (zip, zip3, unzip, unzip3)

-- | Unboxed arrays are represented as unboxed vectors.
--
--   The implementation of `Data.Vector.Unboxed` is based on type families and
--   picks an efficient, specialised representation for every element type. In
--   particular, unboxed vectors of pairs are represented as pairs of unboxed
--   vectors. This is the most efficient representation for numerical data.
--
data U
data instance U.Unbox e => Array U sh e
        = AUnboxed sh !(U.Vector e)
        
deriving instance (Show sh, Show e, U.Unbox e)
        => Show (Array U sh e)

-- Repr -----------------------------------------------------------------------
-- | Read elements from an unboxed vector array.
instance U.Unbox a => Repr U a where
 {-# INLINE linearIndex #-}
 linearIndex (AUnboxed _ vec) ix
        = vec U.! ix

 {-# INLINE unsafeLinearIndex #-}
 unsafeLinearIndex (AUnboxed _ vec) ix
        = vec `U.unsafeIndex` ix

 {-# INLINE extent #-}
 extent (AUnboxed sh _)
        = sh

 {-# INLINE deepSeqArray #-}
 deepSeqArray (AUnboxed sh vec) x 
  = sh `deepSeq` vec `seq` x


-- Fill -----------------------------------------------------------------------
-- | Filling of unboxed vector arrays.
instance U.Unbox e => Fillable U e where
 data MArr U e 
  = UMArr (UM.IOVector e)

 {-# INLINE newMArr #-}
 newMArr n
  = liftM UMArr (UM.new n)

 {-# INLINE unsafeWriteMArr #-}
 unsafeWriteMArr (UMArr v) ix
  = UM.unsafeWrite v ix

 {-# INLINE unsafeFreezeMArr #-}
 unsafeFreezeMArr sh (UMArr mvec)     
  = do  vec     <- U.unsafeFreeze mvec
        return  $  AUnboxed sh vec

 {-# INLINE deepSeqMArr #-}
 deepSeqMArr (UMArr vec) x
  = vec `seq` x


-- Conversions ----------------------------------------------------------------
-- | Sequential computation of array elements..
--
--   * This is an alias for `computeS` with a more specific type.
--
computeUnboxedS
        :: Fill r1 U sh e
        => Array r1 sh e -> Array U sh e
{-# INLINE computeUnboxedS #-}
computeUnboxedS = computeS


-- | Parallel computation of array elements.
--
--   * This is an alias for `computeP` with a more specific type.
--
computeUnboxedP
        :: Fill r1 U sh e
        => Array r1 sh e -> Array U sh e
{-# INLINE computeUnboxedP #-}
computeUnboxedP = computeP


-- | O(n). Convert a list to an unboxed vector array.
-- 
--   * This is an alias for `fromList` with a more specific type.
--
fromListUnboxed
        :: (Shape sh, U.Unbox a)
        => sh -> [a] -> Array U sh a
{-# INLINE fromListUnboxed #-}
fromListUnboxed = R.fromList


-- | O(1). Wrap an unboxed vector as an array.
fromUnboxed
        :: (Shape sh, U.Unbox e)
        => sh -> U.Vector e -> Array U sh e
{-# INLINE fromUnboxed #-}
fromUnboxed sh vec
        = AUnboxed sh vec


-- | O(1). Unpack an unboxed vector from an array.
toUnboxed
        :: U.Unbox e
        => Array U sh e -> U.Vector e
{-# INLINE toUnboxed #-}
toUnboxed (AUnboxed _ vec)
        = vec

-- Zip ------------------------------------------------------------------------
-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip     :: (Shape sh, U.Unbox a, U.Unbox b)
        => Array U sh a -> Array U sh b
        -> Array U sh (a, b)
{-# INLINE zip #-}
zip (AUnboxed sh1 vec1) (AUnboxed sh2 vec2)
 | sh1 /= sh2   = error "Repa: zip array shapes not identical"
 | otherwise    = AUnboxed sh1 (U.zip vec1 vec2)


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip3    :: (Shape sh, U.Unbox a, U.Unbox b, U.Unbox c)
        => Array U sh a -> Array U sh b -> Array U sh c
        -> Array U sh (a, b, c)
{-# INLINE zip3 #-}
zip3 (AUnboxed sh1 vec1) (AUnboxed sh2 vec2) (AUnboxed sh3 vec3)
 | sh1 /= sh2 || sh1 /= sh3
 = error "Repa: zip array shapes not identical"
 | otherwise    = AUnboxed sh1 (U.zip3 vec1 vec2 vec3)


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip4    :: (Shape sh, U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox d)
        => Array U sh a -> Array U sh b -> Array U sh c -> Array U sh d
        -> Array U sh (a, b, c, d)
{-# INLINE zip4 #-}
zip4 (AUnboxed sh1 vec1) (AUnboxed sh2 vec2) (AUnboxed sh3 vec3) (AUnboxed sh4 vec4)
 | sh1 /= sh2 || sh1 /= sh3 || sh1 /= sh4
 = error "Repa: zip array shapes not identical"
 | otherwise    = AUnboxed sh1 (U.zip4 vec1 vec2 vec3 vec4)


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip5    :: (Shape sh, U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox d, U.Unbox e)
        => Array U sh a -> Array U sh b -> Array U sh c -> Array U sh d -> Array U sh e
        -> Array U sh (a, b, c, d, e)
{-# INLINE zip5 #-}
zip5 (AUnboxed sh1 vec1) (AUnboxed sh2 vec2) (AUnboxed sh3 vec3) (AUnboxed sh4 vec4) (AUnboxed sh5 vec5)
 | sh1 /= sh2 || sh1 /= sh3 || sh1 /= sh4 || sh1 /= sh5
 = error "Repa: zip array shapes not identical"
 | otherwise    = AUnboxed sh1 (U.zip5 vec1 vec2 vec3 vec4 vec5)


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip6    :: (Shape sh, U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox d, U.Unbox e, U.Unbox f)
        => Array U sh a -> Array U sh b -> Array U sh c -> Array U sh d -> Array U sh e -> Array U sh f
        -> Array U sh (a, b, c, d, e, f)
{-# INLINE zip6 #-}
zip6 (AUnboxed sh1 vec1) (AUnboxed sh2 vec2) (AUnboxed sh3 vec3) (AUnboxed sh4 vec4) (AUnboxed sh5 vec5) (AUnboxed sh6 vec6)
 | sh1 /= sh2 || sh1 /= sh3 || sh1 /= sh4 || sh1 /= sh5 || sh1 /= sh6
 = error "Repa: zip array shapes not identical"
 | otherwise    = AUnboxed sh1 (U.zip6 vec1 vec2 vec3 vec4 vec5 vec6)
 

-- Unzip ----------------------------------------------------------------------
-- | O(1). Unzip an unboxed array.
unzip   :: (U.Unbox a, U.Unbox b)
        => Array U sh (a, b)
        -> (Array U sh a, Array U sh b)
{-# INLINE unzip #-}
unzip (AUnboxed sh vec)
 = let  (as, bs)        = U.unzip vec
   in   (AUnboxed sh as, AUnboxed sh bs)


-- | O(1). Unzip an unboxed array.
unzip3   :: (U.Unbox a, U.Unbox b, U.Unbox c)
        => Array U sh (a, b, c)
        -> (Array U sh a, Array U sh b, Array U sh c)
{-# INLINE unzip3 #-}
unzip3 (AUnboxed sh vec)
 = let  (as, bs, cs) = U.unzip3 vec
   in   (AUnboxed sh as, AUnboxed sh bs, AUnboxed sh cs)


-- | O(1). Unzip an unboxed array.
unzip4   :: (U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox d)
        => Array U sh (a, b, c, d)
        -> (Array U sh a, Array U sh b, Array U sh c, Array U sh d)
{-# INLINE unzip4 #-}
unzip4 (AUnboxed sh vec)
 = let  (as, bs, cs, ds) = U.unzip4 vec
   in   (AUnboxed sh as, AUnboxed sh bs, AUnboxed sh cs, AUnboxed sh ds)


-- | O(1). Unzip an unboxed array.
unzip5   :: (U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox d, U.Unbox e)
        => Array U sh (a, b, c, d, e)
        -> (Array U sh a, Array U sh b, Array U sh c, Array U sh d, Array U sh e)
{-# INLINE unzip5 #-}
unzip5 (AUnboxed sh vec)
 = let  (as, bs, cs, ds, es) = U.unzip5 vec
   in   (AUnboxed sh as, AUnboxed sh bs, AUnboxed sh cs, AUnboxed sh ds, AUnboxed sh es)


-- | O(1). Unzip an unboxed array.
unzip6  :: (U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox d, U.Unbox e, U.Unbox f)
        => Array U sh (a, b, c, d, e, f)
        -> (Array U sh a, Array U sh b, Array U sh c, Array U sh d, Array U sh e, Array U sh f)
{-# INLINE unzip6 #-}
unzip6 (AUnboxed sh vec)
 = let  (as, bs, cs, ds, es, fs) = U.unzip6 vec
   in   (AUnboxed sh as, AUnboxed sh bs, AUnboxed sh cs, AUnboxed sh ds, AUnboxed sh es, AUnboxed sh fs)
