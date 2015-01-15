
module Data.Repa.IO.Convert
        ( -- * Conversion
          -- | Read and Show `Double`s for a reasonable runtime cost.
          readDouble
        , showDouble
        , showDoubleFixed)
where
import Data.Repa.Array.Foreign
import Data.Repa.Array
import System.IO.Unsafe
import Data.Word
import GHC.Ptr
import qualified Foreign.ForeignPtr                     as F
import qualified Foreign.Storable                       as F
import qualified Foreign.Marshal.Alloc                  as F
import qualified Foreign.Marshal.Utils                  as F
import qualified Data.Double.Conversion.ByteString      as DC


-- | Read a foreign vector as a Double.
--
--   * The standard 'read' function is tragically slow, so shell out and use
--     the C strtod function instead.
--
--   * We take a vector of `Word8` instead of a vector of `Char`
--     as the standard Haskell `Char` is a unicode point that uses 4 whole bytes.
--   
readDouble :: Vector F Word8 -> Double
readDouble (FArray (Z :. len) offset fptr)
 = unsafePerformIO
 $ F.allocaBytes (len + 1) $ \pBuf ->
   F.alloca                $ \pRes ->
   F.withForeignPtr fptr   $ \pIn  -> 
    do  
        -- Copy the data to our new buffer.
        F.copyBytes   pBuf (pIn `plusPtr` offset) (fromIntegral len)

        -- Poke a 0 on the end to ensure it's null terminated.
        F.pokeByteOff pBuf len (0 :: Word8)

        -- Call the C strtod function
        let !d  = strtod pBuf pRes

        return d
{-# NOINLINE readDouble #-}

foreign import ccall unsafe 
 strtod :: Ptr Word8 -> Ptr (Ptr Word8) -> Double


-- | Convert a `Double` to ASCII text packed into a foreign `Vector`.
showDouble :: Double -> Vector F Word8
showDouble !d 
        = fromByteString $ DC.toShortest d
{-# INLINE showDouble #-}


-- | Like `showDouble`, but use a fixed number of digits after
--   the decimal point.
showDoubleFixed :: Int -> Double -> Vector F Word8
showDoubleFixed !prec !d 
        = fromByteString $ DC.toFixed prec d
{-# INLINE showDoubleFixed #-}

