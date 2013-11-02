
module Data.Array.Repa.Series.Vector
        ( Vector        (..)
        , length
        , new, new'
        , tail4, tail8
        , read
        , write
        , writeFloatX4,   writeFloatX8
        , writeDoubleX2,  writeDoubleX4
        , take

          -- * Conversions
        , fromPrimitive
        , toPrimitive)
where
import Data.Array.Repa.Series.Rate
import Data.Array.Repa.Series.Prim.Utils
import Data.Primitive.ByteArray
import Data.Vector.Primitive                    (Prim)
import qualified Data.Vector.Primitive          as P
import qualified Data.Vector.Primitive.Mutable  as PM
import System.IO.Unsafe
import GHC.Exts
import Prelude  hiding (length, read, take, tail)
import Debug.Trace


-- | Abstract mutable vector.
-- 
--   Use `fromPrimitive` and `toPrimitive` to convert to and from
--   Data.Vector.Primitive vectors.
data Vector a
        = Vector
        { vectorLength  :: Word#
        , vectorStart   :: Word#
        , vectorData    :: !(PM.IOVector a) }


instance (Prim a, Show a) => Show (Vector a) where
 show vec 
  = unsafePerformIO
  $ do  fvec    <- P.unsafeFreeze (vectorData vec)
        return  $ show fvec


-- | Take the length of a vector.
length :: Vector a -> Word#
length vec
        = vectorLength vec
{-# INLINE [1] length #-}


-- | Create a new vector of the given length.
new'  :: Prim a => Word# -> IO (Vector a)
new' len
 = do   vec     <- PM.new (I# (word2Int# len))
        return  $ Vector len (int2Word# 0#) vec
{-# INLINE [1] new' #-}


new :: Prim a => Int -> IO (Vector a)
new (I# len)
        = new' (int2Word# len)
{-# INLINE [1] new #-}


-- tail -----------------------------------------------------------------------
-- | Select the tail of a vector.
tail4 :: RateNat (Tail4 k) -> Vector a -> Vector a
tail4 _ (Vector len start vec)
 = let  !start' = quotWord# len (int2Word# 4#) `timesWord#` (int2Word# 4#)
   in   Vector len start' vec
{-# INLINE [1] tail4 #-} 


-- | Select the tail of a vector.
tail8 :: RateNat (Tail8 k) -> Vector a -> Vector a
tail8 _ (Vector len start vec)
 = let  !start' = quotWord# len (int2Word# 8#) `timesWord#` (int2Word# 8#)
   in   Vector len start' vec
{-# INLINE [1] tail8 #-} 


-- read -----------------------------------------------------------------------
-- | Read a value from a vector.
read :: Prim a => Vector a -> Word# -> IO a
read vec ix
 = let  !offset = word2Int# (plusWord# (vectorStart vec) ix)
   in   PM.unsafeRead (vectorData vec) (I# offset)
{-# INLINE [1] read #-}


-- write ----------------------------------------------------------------------
-- | Write a value into a vector.
write :: Prim a => Vector a -> Word# -> a -> IO ()
write vec ix val
 = let  !offset = word2Int# (plusWord# (vectorStart vec) ix)
   in   PM.unsafeWrite (vectorData vec) (I# offset) val
{-# INLINE [1] write #-}


-- | Write a packed FloatX4 into a `Vector`
--
--   You promise that the vectorStart field is zero.
writeFloatX4  :: Vector Float -> Word# -> FloatX4# -> IO ()
writeFloatX4 v ix val
 = let  !(P.MVector (I# start) (I# _) (MutableByteArray mba))
                = vectorData v

        !offset = start +# (word2Int# ix)
   in   wrapIO_ (writeFloatX4Array# mba offset val)
{-# INLINE [1] writeFloatX4 #-}


-- | Write a packed FloatX8 into a `Vector`
--
--   You promise that the vectorStart field is zero.
writeFloatX8  :: Vector Float -> Word# -> FloatX8# -> IO ()
writeFloatX8 v ix val
 = let  !(P.MVector (I# start) (I# _) (MutableByteArray mba))
                = vectorData v

        !offset = start +# (word2Int# ix)
   in   wrapIO_ (writeFloatX8Array# mba offset val)
{-# INLINE [1] writeFloatX8 #-}


-- | Write a packed DoubleX2 into a `Vector`
--
--   You promise that the vectorStart field is zero.
writeDoubleX2  :: Vector Double -> Word# -> DoubleX2# -> IO ()
writeDoubleX2 v ix val
 = let  !(P.MVector (I# start) (I# _) (MutableByteArray mba))
                = vectorData v
        !offset = start +# (word2Int# ix)
   in   wrapIO_ (writeDoubleX2Array# mba offset val)
{-# INLINE [1] writeDoubleX2 #-}


-- | Write a packed DoubleX4 into a `Vector`
--
--   You promise that the vectorStart field is zero.
writeDoubleX4  :: Vector Double -> Word# -> DoubleX4# -> IO ()
writeDoubleX4 v ix val
 = let  !(P.MVector (I# start) (I# _) (MutableByteArray mba))
                = vectorData v
        !offset = start +# (word2Int# ix)
   in   wrapIO_ (writeDoubleX4Array# mba offset val)
{-# INLINE [1] writeDoubleX4 #-}


-- take -----------------------------------------------------------------------
-- | Take the first n elements of a vector
take :: Prim a => Word# -> Vector a -> IO (Vector a)
take len (Vector _ start mvec)
 = do   return  $ Vector len start
                $ PM.unsafeTake (I# (word2Int# len)) mvec
{-# INLINE [1] take #-}


-- to/from primitive ----------------------------------------------------------
-- | O(1). Unsafely convert from an Primitive vector.
--
--   You promise not to access the source vector again.
fromPrimitive :: Prim a => P.Vector a -> IO (Vector a)
fromPrimitive vec
 = do   let !(I# len)   =  P.length vec
        mvec            <- P.unsafeThaw vec
        return $ Vector (int2Word# len) (int2Word# 0#) mvec
{-# INLINE [1] fromPrimitive #-}


-- | O(1). Unsafely convert to an Primitive vector.
--
--   You promise not to modify the source vector again.
toPrimitive :: Prim a => Vector a -> IO (P.Vector a)
toPrimitive (Vector _ _ mvec)
 =      P.unsafeFreeze mvec
{-# INLINE [1] toPrimitive #-}

