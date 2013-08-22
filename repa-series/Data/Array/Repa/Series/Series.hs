
module Data.Array.Repa.Series.Series
        ( Series (..)
        , index
        , length
        , toVector
        , rateOfSeries
        , down4
        , tail4
        , runSeries
        , runSeries2
        , runSeries3
        , runSeries4)
where
import qualified Data.Array.Repa.Series.Vector  as V
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Vector            (Vector)

import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Data.Vector.Unboxed                      (Unbox)
import System.IO.Unsafe
import GHC.Exts
import Prelude hiding (length)


-- Series ---------------------------------------------------------------------
-- | A `Series` is a source of element data that is tagged by rate variable,
--   which is a type level version of its length.
--
--   Although the manifest representation of a series supports random-access
--   indexing, all fusable series process must consume their series sequentially.
--
--   The rate parameter @k@ represents the abstract length of the series.
data Series k a
        = Series 
        { seriesLength  :: Word#
        , seriesVector  :: !(U.Vector a) }  


-- | Index into a series.
index :: Unbox a => Series k a -> Word# -> a
index s ix
        = U.unsafeIndex (seriesVector s) (I# (word2Int# ix))
{-# INLINE [1] index #-}


-- | Take the length of a series.
length :: Series k a -> Word#
length (Series len d) = len
{-# INLINE [1] length #-}


-- | Convert a series to a vector, discarding the rate information.
toVector :: Unbox a => Series k a -> Vector a
toVector (Series len vec) 
 = unsafePerformIO
 $ do   V.fromUnboxed vec
{-# INLINE [1] toVector #-}


-- | Get the Rate / Length of a series.
rateOfSeries :: Series k a -> Word#
rateOfSeries s = seriesLength s
{-# INLINE rateOfSeries #-}


-- | Window a series to the initial range of 4 elements.
down4 :: forall k a. RateNat k -> Series k a -> Series (Down4 k) a
down4 = error "repa-series: down4 not done yet"
{-# NOINLINE down4 #-}


-- | Window a series to the ending elements.
tail4 :: forall k a. RateNat k -> Series k a -> Series (Tail4 k) a
tail4 = error "repa-series: tail4 not done yet"
{-# NOINLINE tail4 #-}


-------------------------------------------------------------------------------
-- | Evaluate a series expression, feeding it an unboxed vector.
--
--   The rate variable @k@ represents the length of the series.
runSeries
        :: Unbox a 
        => Vector a 
        -> (forall k. Series k a -> b)                  -- ^ worker function
        -> b

runSeries vec f
 | len       <- V.length vec
 = unsafePerformIO
 $ do   uvec    <- V.toUnboxed vec
        return  $ f (Series len uvec)
{-# INLINE [1] runSeries #-}


-- | Evaluate a series expression, feeding it two unboxed vectors
--   of the same length.
runSeries2 
        :: (Unbox a, Unbox b)
        => Vector a
        -> Vector b
        -> (forall k. Series k a -> Series k b -> c)    
                        -- ^ worker function
        -> Maybe c

runSeries2 vec1 vec2 f
 | len1      <- V.length vec1
 , len2      <- V.length vec2
 , eqWord# len1 len2
 = unsafePerformIO
 $ do   uvec1   <- V.toUnboxed vec1
        uvec2   <- V.toUnboxed vec2
        return  $ Just (f       (Series len1 uvec1) (Series len2 uvec2))

 | otherwise
 = Nothing
{-# INLINE [1] runSeries2 #-}


-- | Three!
runSeries3 
        :: (Unbox a, Unbox b, Unbox c)
        => Vector a
        -> Vector b
        -> Vector c
        -> (forall k. Series k a -> Series k b -> Series k c -> d)    
                        -- ^ worker function
        -> Maybe d

runSeries3 vec1 vec2 vec3 f
 | len1      <- V.length vec1
 , len2      <- V.length vec2
 , len3      <- V.length vec3
 , eqWord# len1 len2
 , eqWord# len2 len3
 = unsafePerformIO
 $ do   uvec1   <- V.toUnboxed vec1
        uvec2   <- V.toUnboxed vec2
        uvec3   <- V.toUnboxed vec3
        return  $ Just (f       (Series len1 uvec1) (Series len2 uvec2) 
                                (Series len3 uvec3))

 | otherwise
 = Nothing
{-# INLINE [1] runSeries3 #-}


-- | Four!
runSeries4 
        :: (Unbox a, Unbox b, Unbox c, Unbox d)
        => Vector a
        -> Vector b
        -> Vector c
        -> Vector d
        -> (forall k. Series k a -> Series k b -> Series k c -> Series k d -> e)    
                        -- ^ worker function
        -> Maybe e

runSeries4 vec1 vec2 vec3 vec4 f
 | len1      <- V.length vec1
 , len2      <- V.length vec2
 , len3      <- V.length vec3
 , len4      <- V.length vec4
 , eqWord# len1 len2
 , eqWord# len2 len3
 , eqWord# len3 len4
 = unsafePerformIO
 $ do   uvec1   <- V.toUnboxed vec1
        uvec2   <- V.toUnboxed vec2
        uvec3   <- V.toUnboxed vec3
        uvec4   <- V.toUnboxed vec4
        return  $ Just (f       (Series len1 uvec1) (Series len2 uvec2) 
                                (Series len3 uvec3) (Series len4 uvec4))

 | otherwise
 = Nothing
{-# INLINE [1] runSeries4 #-}


