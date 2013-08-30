
module Data.Array.Repa.Series.Series
        ( Series (..)
        , index
        , indexFloatX4
        , indexDoubleX2
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
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Vector            (Vector)
import Data.Vector.Primitive                    (Prim)
import System.IO.Unsafe
import GHC.Exts
import qualified Data.Array.Repa.Series.Vector  as V
import qualified Data.Vector.Primitive          as P
import qualified Data.Vector.Primitive.Mutable  as PM
import Prelude hiding (length)


-- | A `Series` is a source of element data that is tagged by rate variable,
--   which is a type level version of its length.
--
--   Although the manifest representation of a series supports random-access
--   indexing, all fusable series process must consume their series sequentially.
--
--   The rate parameter @k@ represents the abstract length of the series.
--
data Series k a
        = Series 
        { seriesLength  :: Word#
        , seriesVector  :: !(P.Vector a) }  

-- | Take the length of a series.
length :: Series k a -> Word#
length (Series len d) = len
{-# INLINE [1] length #-}


-- | Get the Rate / Length of a series.
rateOfSeries :: Series k a -> RateNat k
rateOfSeries s 
 = RateNat (seriesLength s)
{-# INLINE [1] rateOfSeries #-}


-- | Window a series to the initial range of 4 elements.
down4 :: forall k a. RateNat (Down4 k) -> Series k a -> Series (Down4 k) a
down4 r (Series len vec)        = Series len vec
{-# INLINE [1] down4 #-}


-- | Window a series to the ending elements.
tail4 :: forall k a. RateNat (Tail4 k) -> Series k a -> Series (Tail4 k) a
tail4 r (Series len vec)        = Series len vec
{-# INLINE [1] tail4 #-}


-- | Index into a series.
index :: Prim a => Series k a -> Word# -> a
index s ix
 = P.unsafeIndex (seriesVector s) (I# (word2Int# ix))
{-# INLINE [1] index #-}


-- | Retrieve a packed FloatX4 from a `Series`.
indexFloatX4  :: Series (Down4 k) Float -> Word# -> FloatX4#
indexFloatX4 s ix
        = floatToFloatX4# (int2Float# 5#)                       -- TODO: fixme
{-# INLINE [1] indexFloatX4 #-}


-- | Retrieve a packed DoubleX2 from a `Series`.
indexDoubleX2 :: Series (Down2 k) Float -> Word# -> DoubleX2#
indexDoubleX2 s ix
        = doubleToDoubleX2# (int2Double# 5#)                    -- TODO: fixme
{-# INLINE [1] indexDoubleX2 #-}


-- | Convert a series to a vector, discarding the rate information.
toVector :: Prim a => Series k a -> Vector a
toVector (Series len vec) 
 = unsafePerformIO
 $ do   V.fromPrimitive vec
{-# INLINE [1] toVector #-}


-------------------------------------------------------------------------------
-- | Evaluate a series expression, feeding it an unboxed vector.
--
--   The rate variable @k@ represents the length of the series.
runSeries 
        :: Prim a
        => Vector a 
        -> (forall k. Series k a -> b)  -- ^ worker function
        -> b

runSeries v1 f
 | len       <- V.length v1
 = unsafePerformIO
 $ do   u1      <- V.toPrimitive v1
        return  $ f (Series len u1)
{-# INLINE [1] runSeries #-}


-- | Evaluate a series expression, 
--   feeding it two unboxed vectors of the same length.
runSeries2 
 ::               (Prim a,       Prim b)
 =>              Vector a   -> Vector b
 -> (forall k. Series k a -> Series k b -> c)    
                        -- ^ worker function
 -> Maybe c

runSeries2 v1 v2 f
 | l1 <- V.length v1
 , l2 <- V.length v2, eqWord# l1 l2
 = unsafePerformIO
 $ do   u1      <- V.toPrimitive v1
        u2      <- V.toPrimitive v2
        return  $  Just (f (Series l1 u1) (Series l2 u2))

 | otherwise    = Nothing
{-# INLINE [1] runSeries2 #-}


-- | Three!
runSeries3 
 ::               (Prim a,       Prim b,       Prim c)
 =>            Vector   a -> Vector   b -> Vector   c
 -> (forall k. Series k a -> Series k b -> Series k c -> d)    
 -> Maybe d

runSeries3 v1 v2 v3 f
 | l1 <- V.length v1
 , l2 <- V.length v2, eqWord# l1 l2
 , l3 <- V.length v3, eqWord# l2 l3
 = unsafePerformIO
 $ do   u1      <- V.toPrimitive v1
        u2      <- V.toPrimitive v2
        u3      <- V.toPrimitive v3
        return  $ Just (f (Series l1 u1) (Series l2 u2) 
                          (Series l3 u3))
 | otherwise = Nothing
{-# INLINE [1] runSeries3 #-}


-- | Four!
runSeries4 
 ::               (Prim a,       Prim b,       Prim c,       Prim d)
 =>            Vector   a -> Vector   b -> Vector   c -> Vector   d
 -> (forall k. Series k a -> Series k b -> Series k c -> Series k d -> e)
 -> Maybe e

runSeries4 v1 v2 v3 v4 f
 | l1 <- V.length v1
 , l2 <- V.length v2,   eqWord# l1 l2
 , l3 <- V.length v3,   eqWord# l2 l3
 , l4 <- V.length v4,   eqWord# l3 l4
 = unsafePerformIO
 $ do   u1      <- V.toPrimitive v1
        u2      <- V.toPrimitive v2
        u3      <- V.toPrimitive v3
        u4      <- V.toPrimitive v4
        return  $ Just (f (Series l1 u1) (Series l2 u2) 
                          (Series l3 u3) (Series l4 u4))
 | otherwise = Nothing
{-# INLINE [1] runSeries4 #-}

