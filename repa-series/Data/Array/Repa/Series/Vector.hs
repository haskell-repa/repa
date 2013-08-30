
module Data.Array.Repa.Series.Vector
        ( Vector
        , length
        , new
        , read
        , write
        , take

          -- * Conversions
        , fromPrimitive
        , toPrimitive)
where
import Data.Vector.Primitive                    (Prim)
import qualified Data.Vector.Primitive          as P
import qualified Data.Vector.Primitive.Mutable  as PM
import System.IO.Unsafe
import GHC.Exts
import Prelude  hiding (length, read, take)


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
{-# INLINE length #-}


-- | Create a new vector of the given length.
new  :: Prim a => Word# -> IO (Vector a)
new len
 = do   vec     <- PM.new (I# (word2Int# len))
        return  $ Vector len vec
{-# INLINE new #-}


-- | Read a value from a vector.
read :: Prim a => Vector a -> Word# -> IO a
read vec ix
        = PM.unsafeRead (vectorData vec) (I# (word2Int# ix))
{-# INLINE read #-}


-- | Write a value into a vector.
write :: Prim a => Vector a -> Word# -> a -> IO ()
write vec ix val
        = PM.unsafeWrite (vectorData vec) (I# (word2Int# ix)) val
{-# INLINE write #-}


-- | Take the first n elements of a vector
take :: Prim a => Word# -> Vector a -> IO (Vector a)
take len (Vector _ mvec)
 = do   return  $ Vector len 
                $ PM.unsafeTake (I# (word2Int# len)) mvec
{-# INLINE take #-}


-- | O(1). Unsafely convert from an Primitive vector.
--
--   You promise not to access the source vector again.
fromPrimitive :: Prim a => P.Vector a -> IO (Vector a)
fromPrimitive vec
 = do   let !(I# len)   =  P.length vec
        mvec            <- P.unsafeThaw vec
        return $ Vector (int2Word# len) mvec
{-# INLINE fromPrimitive #-}


-- | O(1). Unsafely convert to an Primitive vector.
--
--   You promise not to modify the source vector again.
toPrimitive :: Prim a => Vector a -> IO (P.Vector a)
toPrimitive (Vector _ mvec)
 =      P.unsafeFreeze mvec
{-# INLINE toPrimitive #-}

