
module Data.Array.Repa.Series.Vector
        ( Vector        (..)
        , length
        , new, new'
        , read
        , write
        , writeFloatX4
        , writeDoubleX2
        , take

          -- * Conversions
        , fromPrimitive
        , toPrimitive)
where
import Data.Array.Repa.Series.Prim.Utils
import Data.Primitive.ByteArray
import Data.Vector.Primitive                    (Prim)
import qualified Data.Vector.Primitive          as P
import qualified Data.Vector.Primitive.Mutable  as PM
import System.IO.Unsafe
import GHC.Exts
import Prelude  hiding (length, read, take)
import Debug.Trace

-- | Abstract mutable vector.
-- 
--   Use `fromPrimitive` and `toPrimitive` to convert to and from
--   Data.Vector.Primitive vectors.
data Vector a
        = Vector
        { vectorLength  :: Word#
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
        return  $ Vector len vec
{-# INLINE [1] new' #-}


new :: Prim a => Int -> IO (Vector a)
new (I# len)
        = new' (int2Word# len)
{-# INLINE [1] new #-}


-- | Read a value from a vector.
read :: Prim a => Vector a -> Word# -> IO a
read vec ix
        = PM.unsafeRead (vectorData vec) (I# (word2Int# ix))
{-# INLINE [1] read #-}


-- | Write a value into a vector.
write :: Prim a => Vector a -> Word# -> a -> IO ()
write vec ix val
 = let  !offset = word2Int# ix
   in   PM.unsafeWrite (vectorData vec) (I# offset) val
{-# INLINE [1] write #-}


-- | Write a packed FloatX4 into a `Vector`
writeFloatX4  :: Vector Float -> Word# -> FloatX4# -> IO ()
writeFloatX4 v ix val
 = let  !(P.MVector (I# start) (I# _) (MutableByteArray mba))
                = vectorData v

        !offset = start +# (word2Int# ix)
   in   wrapIO_ (writeFloatX4Array# mba offset val)
{-# INLINE [1] writeFloatX4 #-}


-- | Write a packed DoubleX2 into a `Vector`
writeDoubleX2  :: Vector Double -> Word# -> DoubleX2# -> IO ()
writeDoubleX2 v ix val
 = let  !(P.MVector (I# start) (I# _) (MutableByteArray mba))
                = vectorData v
        !offset = start +# (word2Int# ix)
   in   wrapIO_ (writeDoubleX2Array# mba offset val)
{-# INLINE [1] writeDoubleX2 #-}


-- | Take the first n elements of a vector
take :: Prim a => Word# -> Vector a -> IO (Vector a)
take len (Vector _ mvec)
 = do   return  $ Vector len 
                $ PM.unsafeTake (I# (word2Int# len)) mvec
{-# INLINE [1] take #-}


-- | O(1). Unsafely convert from an Primitive vector.
--
--   You promise not to access the source vector again.
fromPrimitive :: Prim a => P.Vector a -> IO (Vector a)
fromPrimitive vec
 = do   let !(I# len)   =  P.length vec
        mvec            <- P.unsafeThaw vec
        return $ Vector (int2Word# len) mvec
{-# INLINE [1] fromPrimitive #-}


-- | O(1). Unsafely convert to an Primitive vector.
--
--   You promise not to modify the source vector again.
toPrimitive :: Prim a => Vector a -> IO (P.Vector a)
toPrimitive (Vector _ mvec)
 =      P.unsafeFreeze mvec
{-# INLINE [1] toPrimitive #-}

