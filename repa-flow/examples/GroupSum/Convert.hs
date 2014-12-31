
{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module Convert
        ( readDouble
        , showDouble
        , showDoubleFixed)
where
import Control.Monad.Primitive
import GHC.Ptr
import Data.Word
import Data.ByteString.Char8                            (ByteString)

import qualified Foreign.ForeignPtr                     as F
import qualified Foreign.Storable                       as F
import qualified Foreign.Marshal.Alloc                  as F
import qualified Foreign.Marshal.Utils                  as F
import qualified Data.ByteString.Internal               as BSI
import qualified Data.Double.Conversion.ByteString      as DC


-- | Read a Bytestring as a Double.
--   The standard 'read' function is tragically slow, so shell out and use
--   the C strtod function instead.
readDouble :: ByteString -> Double
readDouble (BSI.PS fp offset len)
 = unsafeInlineIO
 $ F.allocaBytes (len + 1) $ \pBuf ->
   F.alloca                $ \pRes ->
   F.withForeignPtr fp     $ \pBS  -> 
    do  
        -- Copy the bytestring data to our new buffer.
        F.copyBytes pBuf (pBS `plusPtr` offset) (fromIntegral len)

        -- Poke a 0 on the end to ensure it's null terminated.
        F.pokeByteOff pBuf len (0 :: Word8)

        -- Call the C strtod function
        let !d  = strtod pBS pRes

        return d
{-# INLINE readDouble #-}

foreign import ccall unsafe 
 strtod :: Ptr Word8 -> Ptr (Ptr Word8) -> Double


-- | Convert a double to a bytestring.
showDouble :: Double -> ByteString
showDouble !d = DC.toShortest d
{-# INLINE showDouble #-}


-- | Convert a double to a Bytestring using a fixed number of digits
--   after the decimal point.
showDoubleFixed :: Int -> Double -> ByteString
showDoubleFixed !prec !d = DC.toFixed prec d
{-# INLINE showDoubleFixed #-}
