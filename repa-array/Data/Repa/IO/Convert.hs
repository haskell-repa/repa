
module Data.Repa.IO.Convert
        ( -- * Conversion
          -- | Read and Show `Double`s for a reasonable runtime cost.
          readDouble,           readDoubleFromBytes
        , showDouble,           showDoubleAsBytes
        , showDoubleFixed,      showDoubleFixedAsBytes)
where
import Data.Repa.Array.Material.Safe.Foreign            as A
import Data.Repa.Array                                  as A
import System.IO.Unsafe
import Data.Word
import Data.Char
import GHC.Ptr
import qualified Foreign.ForeignPtr                     as F
import qualified Foreign.Storable                       as F
import qualified Foreign.Marshal.Alloc                  as F
import qualified Foreign.Marshal.Utils                  as F
import qualified Data.Double.Conversion.ByteString      as DC


-- | Convert a foreign vector of characters to a Double.
-- 
--   * The standard Haskell `Char` type is four bytes in length.
--     If you already have a vector of `Word8` then use `readDoubleFromBytes`
--     instead to avoid the conversion.
--
readDouble :: Vector F Char -> Double
readDouble vec
 = readDoubleFromBytes $ A.computeS_ $ A.map (fromIntegral . ord) vec
{-# INLINE readDouble #-}


-- | Convert a foreign vector of bytes to a Double.
readDoubleFromBytes :: Vector F Word8 -> Double
readDoubleFromBytes (FArray (Z :. len) offset fptr)
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
{-# NOINLINE readDoubleFromBytes #-}

foreign import ccall unsafe 
 strtod :: Ptr Word8 -> Ptr (Ptr Word8) -> Double


-- | Convert a `Double` to ASCII text packed into a foreign `Vector`.
showDouble :: Double -> Vector F Char
showDouble !d
        = A.computeS_ $ A.map (chr . fromIntegral)
        $ showDoubleAsBytes d

-- | Convert a `Double` to ASCII text packed into a foreign `Vector`.
showDoubleAsBytes :: Double -> Vector F Word8
showDoubleAsBytes !d 
        = fromByteString $ DC.toShortest d
{-# INLINE showDouble #-}


-- | Like `showDouble`, but use a fixed number of digits after
--   the decimal point.
showDoubleFixed :: Int -> Double -> Vector F Char
showDoubleFixed !prec !d
        = A.computeS_ $ A.map (chr . fromIntegral)
        $ showDoubleFixedAsBytes prec d
{-# INLINE showDoubleFixed #-}


-- | Like `showDoubleAsBytes`, but use a fixed number of digits after
--   the decimal point.
showDoubleFixedAsBytes :: Int -> Double -> Vector F Word8
showDoubleFixedAsBytes !prec !d 
        = fromByteString $ DC.toFixed prec d
{-# INLINE showDoubleFixedAsBytes #-}

