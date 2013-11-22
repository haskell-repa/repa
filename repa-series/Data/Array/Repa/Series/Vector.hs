
module Data.Array.Repa.Series.Vector
        ( Vector        (..)
        , lengthData
        , new, new'
        , tail4, tail8
        , read
        , write
        , writeFloatX4,   writeFloatX8
        , writeDoubleX2,  writeDoubleX4
        , take
        , trunc

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


-- | Abstract mutable vector, used for flow fusion.
--
--   WARNING: These are not general purpose vectors. Their representation has 
--   been optimised for use with flow fusion, and some functions may not behave
--   as expected. In particular, the 'tail4/8' functions will not give sensible
--   results after applying 'trunc'. We intend 'trunc' to be the LAST
--   operation on a vector before extracting the underling buffer with the 
--   toPrimitive function.
-- 
--   Use `fromPrimitive` and `toPrimitive` to convert to and from
--   Data.Vector.Primitive vectors.
data Vector a
        = Vector
        { -- | Physical length of the underling data buffer.
          vectorLengthData      :: Word#

          -- | Logical length of the vector,
          --   must be smaller than the `vectorLengthData`.
        , vectorLengthTrunc     :: !(PM.IOVector Word)

          -- | Starting position in the underlying data buffer.
        , vectorStart           :: Word#

          -- | Data buffer.
        , vectorData            :: !(PM.IOVector a) }


instance (Prim a, Show a) => Show (Vector a) where
 show vec 
  = unsafePerformIO
  $ do  pv       <- toPrimitive vec
        return  $ show pv


-- | Get the length of the underlying data buffer.
lengthData :: Vector a -> Word#
lengthData vec
        = vectorLengthData vec
{-# INLINE [1] lengthData #-}


-- | Create a new vector of the given length.
--   Both the data length and trunc length are set to the given value.
new'  :: Prim a => Word# -> IO (Vector a)
new' lenBuf
 = do   vec      <- PM.new (I# (word2Int# lenBuf))

        lenTrunc <- PM.new 1
        PM.unsafeWrite lenTrunc 0 (W# lenBuf)

        return  $ Vector lenBuf lenTrunc (int2Word# 0#) vec
{-# INLINE [1] new' #-}


new :: Prim a => Int -> IO (Vector a)
new (I# len)
        = new' (int2Word# len)
{-# INLINE [1] new #-}


-- tail -----------------------------------------------------------------------
-- | Select the tail of a vector.
tail4 :: RateNat (Tail4 k) -> Vector a -> Vector a
tail4 _ (Vector lenBuf lenTrunc start vec)
 = let  !start' = quotWord# lenBuf (int2Word# 4#) `timesWord#` (int2Word# 4#)
   in   Vector lenBuf lenTrunc start' vec
{-# INLINE [1] tail4 #-} 


-- | Select the tail of a vector.
tail8 :: RateNat (Tail8 k) -> Vector a -> Vector a
tail8 _ (Vector lenBuf lenTrunc start vec)
 = let  !start' = quotWord# lenBuf (int2Word# 8#) `timesWord#` (int2Word# 8#)
   in   Vector lenBuf lenTrunc start' vec
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
take len (Vector _ lenTrunc start mvec)
 = do   return  $ Vector len lenTrunc start
                $ PM.unsafeTake (I# (word2Int# len)) mvec
{-# INLINE [1] take #-}


-- trunc ----------------------------------------------------------------------
-- | Set the truncLength of a vector.
--
--   WARNING: This is intended to be the LAST operation before a toPrimitive
--   the 'tail4/tail8' and 'take' functions may not behave as expected after
--   doing this.
--
trunc :: Word# -> Vector a -> IO ()
trunc lenNew (Vector _ lenTrunc _ _)
 = do   
        PM.unsafeWrite lenTrunc 0 (W# lenNew)
{-# INLINE [1] trunc #-}


-- to/from primitive ----------------------------------------------------------
-- | O(1). Unsafely convert from an Primitive vector.
--
--   You promise not to access the source vector again.
fromPrimitive :: Prim a => P.Vector a -> IO (Vector a)
fromPrimitive vec
 = do   let !(I# lenBuf) =  P.length vec

        lenTrunc <- PM.new 1
        PM.unsafeWrite lenTrunc 0 (W# (int2Word# lenBuf))

        mvec            <- P.unsafeThaw vec
        return $ Vector (int2Word# lenBuf) lenTrunc (int2Word# 0#) mvec
{-# INLINE [1] fromPrimitive #-}


-- | O(1). Unsafely convert to an Primitive vector.
--
--   You promise not to modify the source vector again.
toPrimitive :: Prim a => Vector a -> IO (P.Vector a)
toPrimitive (Vector _ lenTrunc _ mvec)
 = do   len       <- PM.unsafeRead lenTrunc 0
        let mvec' =  PM.unsafeSlice 0 (fromIntegral len) mvec
        P.unsafeFreeze mvec'
{-# INLINE [1] toPrimitive #-}

