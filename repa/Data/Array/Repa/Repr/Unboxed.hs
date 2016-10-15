
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
--   The implementation uses @Data.Vector.Unboxed@ which is based on type
--   families and picks an efficient, specialised representation for every
--   element type. In particular, unboxed vectors of pairs are represented
--   as pairs of unboxed vectors.
--   This is the most efficient representation for numerical data.
--
data U

-- | Read elements from an unboxed vector array.
instance U.Unbox a => Source U a where
 data Array U sh a
        = AUnboxed !sh !(U.Vector a)

 linearIndex (AUnboxed _ vec) ix
        = vec U.! ix
 {-# INLINE linearIndex #-}

 unsafeLinearIndex (AUnboxed _ vec) ix
        = vec `U.unsafeIndex` ix
 {-# INLINE unsafeLinearIndex #-}

 extent (AUnboxed sh _)
        = sh
 {-# INLINE extent #-}

 deepSeqArray (AUnboxed sh vec) x 
  = sh `deepSeq` vec `seq` x
 {-# INLINE deepSeqArray #-}


deriving instance (Show sh, Show e, U.Unbox e)
        => Show (Array U sh e)

deriving instance (Read sh, Read e, U.Unbox e)
        => Read (Array U sh e)


-- Fill -----------------------------------------------------------------------
-- | Filling of unboxed vector arrays.
instance U.Unbox e => Target U e where
 data MVec U e 
  = UMVec (UM.IOVector e)

 newMVec n
  = liftM UMVec (UM.new n)
 {-# INLINE newMVec #-}

 unsafeWriteMVec (UMVec v) ix
  = UM.unsafeWrite v ix
 {-# INLINE unsafeWriteMVec #-}

 unsafeFreezeMVec sh (UMVec mvec)     
  = do  vec     <- U.unsafeFreeze mvec
        return  $  AUnboxed sh vec
 {-# INLINE unsafeFreezeMVec #-}

 deepSeqMVec (UMVec vec) x
  = vec `seq` x
 {-# INLINE deepSeqMVec #-}

 touchMVec _ 
  = return ()
 {-# INLINE touchMVec #-}


-- Conversions ----------------------------------------------------------------
-- | Sequential computation of array elements..
--
--   * This is an alias for `computeS` with a more specific type.
--
computeUnboxedS
        :: (Load r1 sh e, U.Unbox e)
        => Array r1 sh e -> Array U sh e
computeUnboxedS = computeS
{-# INLINE computeUnboxedS #-}


-- | Parallel computation of array elements.
--
--   * This is an alias for `computeP` with a more specific type.
--
computeUnboxedP
        :: (Load r1 sh e, Monad m, U.Unbox e)
        => Array r1 sh e -> m (Array U sh e)
computeUnboxedP = computeP
{-# INLINE computeUnboxedP #-}


-- | O(n). Convert a list to an unboxed vector array.
-- 
--   * This is an alias for `fromList` with a more specific type.
--
fromListUnboxed
        :: (Shape sh, U.Unbox a)
        => sh -> [a] -> Array U sh a
fromListUnboxed = R.fromList
{-# INLINE fromListUnboxed #-}


-- | O(1). Wrap an unboxed vector as an array.
fromUnboxed :: sh -> U.Vector e -> Array U sh e
fromUnboxed sh vec
        = AUnboxed sh vec
{-# INLINE fromUnboxed #-}


-- | O(1). Unpack an unboxed vector from an array.
toUnboxed :: Array U sh e -> U.Vector e
toUnboxed (AUnboxed _ vec)
        = vec
{-# INLINE toUnboxed #-}


-- Zip ------------------------------------------------------------------------
-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip     :: (Shape sh, U.Unbox a, U.Unbox b)
        => Array U sh a -> Array U sh b
        -> Array U sh (a, b)
zip (AUnboxed sh1 vec1) (AUnboxed sh2 vec2)
 | sh1 /= sh2   = error "Repa: zip array shapes not identical"
 | otherwise    = AUnboxed sh1 (U.zip vec1 vec2)
{-# INLINE zip #-}


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip3    :: (Shape sh, U.Unbox a, U.Unbox b, U.Unbox c)
        => Array U sh a -> Array U sh b -> Array U sh c
        -> Array U sh (a, b, c)
zip3 (AUnboxed sh1 vec1) (AUnboxed sh2 vec2) (AUnboxed sh3 vec3)
 | sh1 /= sh2 || sh1 /= sh3
 = error "Repa: zip array shapes not identical"
 | otherwise    = AUnboxed sh1 (U.zip3 vec1 vec2 vec3)
{-# INLINE zip3 #-}


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip4    :: (Shape sh, U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox d)
        => Array U sh a -> Array U sh b -> Array U sh c -> Array U sh d
        -> Array U sh (a, b, c, d)
zip4 (AUnboxed sh1 vec1) (AUnboxed sh2 vec2) (AUnboxed sh3 vec3) (AUnboxed sh4 vec4)
 | sh1 /= sh2 || sh1 /= sh3 || sh1 /= sh4
 = error "Repa: zip array shapes not identical"
 | otherwise    = AUnboxed sh1 (U.zip4 vec1 vec2 vec3 vec4)
{-# INLINE zip4 #-}


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip5    :: (Shape sh, U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox d, U.Unbox e)
        => Array U sh a -> Array U sh b -> Array U sh c -> Array U sh d -> Array U sh e
        -> Array U sh (a, b, c, d, e)
zip5 (AUnboxed sh1 vec1) (AUnboxed sh2 vec2) (AUnboxed sh3 vec3) (AUnboxed sh4 vec4) (AUnboxed sh5 vec5)
 | sh1 /= sh2 || sh1 /= sh3 || sh1 /= sh4 || sh1 /= sh5
 = error "Repa: zip array shapes not identical"
 | otherwise    = AUnboxed sh1 (U.zip5 vec1 vec2 vec3 vec4 vec5)
{-# INLINE zip5 #-}


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip6    :: (Shape sh, U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox d, U.Unbox e, U.Unbox f)
        => Array U sh a -> Array U sh b -> Array U sh c -> Array U sh d -> Array U sh e -> Array U sh f
        -> Array U sh (a, b, c, d, e, f)
zip6 (AUnboxed sh1 vec1) (AUnboxed sh2 vec2) (AUnboxed sh3 vec3) (AUnboxed sh4 vec4) (AUnboxed sh5 vec5) (AUnboxed sh6 vec6)
 | sh1 /= sh2 || sh1 /= sh3 || sh1 /= sh4 || sh1 /= sh5 || sh1 /= sh6
 = error "Repa: zip array shapes not identical"
 | otherwise    = AUnboxed sh1 (U.zip6 vec1 vec2 vec3 vec4 vec5 vec6)
{-# INLINE zip6 #-}
 

-- Unzip ----------------------------------------------------------------------
-- | O(1). Unzip an unboxed array.
unzip   :: (U.Unbox a, U.Unbox b)
        => Array U sh (a, b)
        -> (Array U sh a, Array U sh b)
unzip (AUnboxed sh vec)
 = let  (as, bs)        = U.unzip vec
   in   (AUnboxed sh as, AUnboxed sh bs)
{-# INLINE unzip #-}


-- | O(1). Unzip an unboxed array.
unzip3   :: (U.Unbox a, U.Unbox b, U.Unbox c)
        => Array U sh (a, b, c)
        -> (Array U sh a, Array U sh b, Array U sh c)
unzip3 (AUnboxed sh vec)
 = let  (as, bs, cs) = U.unzip3 vec
   in   (AUnboxed sh as, AUnboxed sh bs, AUnboxed sh cs)
{-# INLINE unzip3 #-}


-- | O(1). Unzip an unboxed array.
unzip4   :: (U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox d)
        => Array U sh (a, b, c, d)
        -> (Array U sh a, Array U sh b, Array U sh c, Array U sh d)
unzip4 (AUnboxed sh vec)
 = let  (as, bs, cs, ds) = U.unzip4 vec
   in   (AUnboxed sh as, AUnboxed sh bs, AUnboxed sh cs, AUnboxed sh ds)
{-# INLINE unzip4 #-}


-- | O(1). Unzip an unboxed array.
unzip5   :: (U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox d, U.Unbox e)
        => Array U sh (a, b, c, d, e)
        -> (Array U sh a, Array U sh b, Array U sh c, Array U sh d, Array U sh e)
unzip5 (AUnboxed sh vec)
 = let  (as, bs, cs, ds, es) = U.unzip5 vec
   in   (AUnboxed sh as, AUnboxed sh bs, AUnboxed sh cs, AUnboxed sh ds, AUnboxed sh es)
{-# INLINE unzip5 #-}


-- | O(1). Unzip an unboxed array.
unzip6  :: (U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox d, U.Unbox e, U.Unbox f)
        => Array U sh (a, b, c, d, e, f)
        -> (Array U sh a, Array U sh b, Array U sh c, Array U sh d, Array U sh e, Array U sh f)
unzip6 (AUnboxed sh vec)
 = let  (as, bs, cs, ds, es, fs) = U.unzip6 vec
   in   (AUnboxed sh as, AUnboxed sh bs, AUnboxed sh cs, AUnboxed sh ds, AUnboxed sh es, AUnboxed sh fs)
{-# INLINE unzip6 #-}
