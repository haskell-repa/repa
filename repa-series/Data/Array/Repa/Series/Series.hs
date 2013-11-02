
module Data.Array.Repa.Series.Series
        ( Series (..)
        , index
        , indexFloatX4,  indexFloatX8
        , indexDoubleX2, indexDoubleX4
        , length
        , toVector
        , rateOfSeries
        , down4,        down8
        , tail4,        tail8
        , unsafeFromVector)
where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Vector            (Vector)
import Data.Vector.Primitive                    (Prim)
import Data.Primitive.ByteArray
import System.IO.Unsafe
import GHC.Exts
import GHC.Prim
import qualified Data.Array.Repa.Series.Vector  as V
import qualified Data.Vector.Primitive          as P
import qualified Data.Vector.Primitive.Mutable  as PM
import Prelude hiding (length)
import Debug.Trace
import Control.Monad

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
        { -- | Total length of the series.
          seriesLength          :: Word#

          -- | Starting point in the series, 
          --   ignore this many elements from the front.
        , seriesStart           :: Word#

          -- | Byte array holding series data.
        , seriesByteArray       :: ByteArray#

          -- | Byte array wrapped into a primitive vector,
          --   for ease of access.
        , seriesVector          :: !(P.Vector a) }  


-- | Take the length of a series.
length :: Series k a -> Word#
length s        = seriesLength s
{-# INLINE [1] length #-}


-- | Get the Rate / Length of a series.
rateOfSeries :: Series k a -> RateNat k
rateOfSeries s  = RateNat (seriesLength s)
{-# INLINE [1] rateOfSeries #-}


-- down -----------------------------------------------------------------------
-- | Window a series to the initial range of 4 elements.
down4 :: forall k a. RateNat (Down4 k) -> Series k a -> Series (Down4 k) a
down4 r (Series len start ba vec)        
 = let  !len'   = minusWord# len (remWord# len (int2Word# 4#))
   in   Series len' start ba vec
{-# INLINE [1] down4 #-}


-- | Window a series to the initial range of 8 elements.
down8 :: forall k a. RateNat (Down8 k) -> Series k a -> Series (Down8 k) a
down8 r (Series len start ba vec)        
 = let  !len'   = minusWord# len (remWord# len (int2Word# 8#))
   in   Series len' start ba vec
{-# INLINE [1] down8 #-}


-- tail -----------------------------------------------------------------------
-- | Window a series to the ending elements.
tail4 :: forall k a. RateNat (Tail4 k) -> Series k a -> Series (Tail4 k) a
tail4 r (Series len start ba vec)        
 = let  !start' = quotWord# len (int2Word# 4#) `timesWord#` (int2Word# 4#)
   in   Series len start' ba vec
{-# INLINE [1] tail4 #-}


-- | Window a series to the ending elements.
tail8 :: forall k a. RateNat (Tail8 k) -> Series k a -> Series (Tail8 k) a
tail8 r (Series len start ba vec)        
 = let  !start' = quotWord# len (int2Word# 8#) `timesWord#` (int2Word# 8#)
   in   Series len start' ba vec
{-# INLINE [1] tail8 #-}


-- index ----------------------------------------------------------------------
-- | Index into a series.
index :: Prim a => Series k a -> Word# -> a
index s ix
 = let  !offset         = word2Int# (ix `plusWord#` seriesStart s)
   in   P.unsafeIndex (seriesVector s) (I# offset)
{-# INLINE [1] index #-}


-- | Retrieve a packed FloatX4# from a `Series`.
indexFloatX4  :: Series (Down4 k) Float -> Word# -> FloatX4#
indexFloatX4 s ix
 = let  !ba             = seriesByteArray s
        !offset         = word2Int# (plusWord# (seriesStart s) ix)
   in   indexFloatX4Array# ba offset
{-# INLINE [1] indexFloatX4 #-}


-- | Retrieve a packed FloatX8# from a `Series`.
indexFloatX8  :: Series (Down8 k) Float -> Word# -> FloatX8#
indexFloatX8 s ix
 = let  !ba             = seriesByteArray s
        !offset         = word2Int# (plusWord# (seriesStart s) ix)
   in   indexFloatX8Array# ba offset
{-# INLINE [1] indexFloatX8 #-}


-- | Retrieve a packed DoubleX2# from a `Series`.
indexDoubleX2 :: Series (Down2 k) Float -> Word# -> DoubleX2#
indexDoubleX2 s ix
 = let  !ba             = seriesByteArray s
        !offset         = word2Int# (plusWord# (seriesStart s) ix)
   in   indexDoubleX2Array# ba offset 
{-# INLINE [1] indexDoubleX2 #-}


-- | Retrieve a packed DoubleX4# from a `Series`.
indexDoubleX4 :: Series (Down4 k) Float -> Word# -> DoubleX4#
indexDoubleX4 s ix
 = let  !ba             = seriesByteArray s
        !offset         = word2Int# (plusWord# (seriesStart s) ix)
   in   indexDoubleX4Array# ba offset 
{-# INLINE [1] indexDoubleX4 #-}


-- to/from vector -------------------------------------------------------------
-- | Convert a series to a vector, discarding the rate information.
toVector :: Prim a => Series k a -> Vector a
toVector !s
 = unsafePerformIO
 $ do   V.fromPrimitive (seriesVector s)
{-# NOINLINE toVector #-}


-- | Unsafely convert a vector to a series of an arbitrary rate.
--   
--   The rate variable in the result is arbitrary,
--   so a series created this way may not have the same length as others
--   of the same rate.
unsafeFromVector :: Prim a => Vector a -> IO (Series k a)
unsafeFromVector (V.Vector len start mv)
 = do   let !pv@(P.MVector baStart (I# _) (MutableByteArray mba))
                        = mv
        v               <- P.unsafeFreeze mv
        let (# _, ba #) =  unsafeFreezeByteArray# mba realWorld# 
        when (baStart /= 0)
         $ error "unsafeFromVector: vector has non-zero prim starting offset"
        return $ Series len start ba v
{-# NOINLINE unsafeFromVector #-}

