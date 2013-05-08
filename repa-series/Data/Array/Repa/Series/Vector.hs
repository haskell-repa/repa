
module Data.Array.Repa.Series.Vector
        ( Vector
        , length
        , new
        , read
        , write

          -- * Conversions
        , fromUnboxed
        , toUnboxed)
where
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Data.Vector.Unboxed                      (Unbox)
import System.IO.Unsafe
import GHC.Exts
import Prelude  hiding (length, read)


data Vector a
        = Vector
        { vectorLength  :: Int#
        , vectorData    :: !(UM.IOVector a) }


-- | Take the length of a vector.
length :: Vector a -> Int#
length vec
        = vectorLength vec
{-# INLINE length #-}


-- | Create a new vector of the given length.
new  :: Unbox a => Int# -> IO (Vector a)
new len
 = do   vec     <- UM.new (I# len)
        return  $ Vector len vec
{-# INLINE new #-}


-- | Read a value from a vector.
read :: Unbox a => Vector a -> Int# -> IO a
read vec ix
        = UM.unsafeRead (vectorData vec) (I# ix)
{-# INLINE read #-}


-- | Write a value into a vector.
write :: Unbox a => Vector a -> Int# -> a -> IO ()
write vec ix val
        = UM.unsafeWrite (vectorData vec) (I# ix) val
{-# INLINE write #-}


-- | Convert from an Unboxed vector.
fromUnboxed :: Unbox a => U.Vector a -> IO (Vector a)
fromUnboxed vec
 = do   let !(I# len)   = U.length vec
        mvec            <- U.unsafeThaw vec
        return $ Vector len mvec
{-# INLINE fromUnboxed #-}


-- | Convert to an Unboxed vector.
toUnboxed :: Unbox a => Vector a -> IO (U.Vector a)
toUnboxed (Vector _ mvec)
 =      U.unsafeFreeze mvec
{-# INLINE toUnboxed #-}
