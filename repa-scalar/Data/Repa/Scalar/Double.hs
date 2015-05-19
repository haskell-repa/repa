
-- | Loading and storing doubles directly from/to memory buffers.
module Data.Repa.Scalar.Double
        ( -- * Loading
          loadDouble

          -- * Storing
        , storeDoubleShortest
        , storeDoubleFixed)
where
import Data.Word
import GHC.Exts
import qualified Data.ByteString.Internal               as BS
import qualified Data.Double.Conversion.ByteString      as DC
import qualified Foreign.Ptr                            as F
import qualified Foreign.ForeignPtr                     as F
import qualified Foreign.Storable                       as F
import qualified Foreign.Marshal.Alloc                  as F
import qualified Foreign.Marshal.Utils                  as F


-- Double -----------------------------------------------------------------------------------------
-- | Load an ASCII `Double` from a foreign buffer
--   returning the value and number of characters read.
loadDouble 
        :: Ptr Word8            -- ^ Buffer holding ASCII representation.
        -> Int                  -- ^ Length of buffer.
        -> IO (Double, Int)     -- ^ Result, and number of characters read from buffer.

loadDouble !pIn !len
 = F.allocaBytes (len + 1) $ \pBuf ->
   F.alloca                $ \pRes ->
    do
        -- Copy the data to our new buffer.
        F.copyBytes pBuf pIn (fromIntegral len)

        -- Poke a 0 on the end to ensure it's null terminated.
        F.pokeByteOff pBuf len (0 :: Word8)

        -- Call the C strtod function
        let !d  = strtod pBuf pRes

        -- Read back the end pointer.
        res     <- F.peek pRes

        return (d, res `F.minusPtr` pBuf)
{-# INLINE loadDouble #-}


-- TODO: strtod will skip whitespace before the actual double, 
-- but we probably want to avoid this to be consistent.
foreign import ccall unsafe
 strtod :: Ptr Word8 -> Ptr (Ptr Word8) -> Double



-- | Store an ASCII `Double`, yielding a freshly allocated buffer
--   and its length.
--
--   * The value is printed as either (sign)digits.digits,
--     or in exponential format, depending on which is shorter.
--
--   * The result is buffer not null terminated.
--
storeDoubleShortest :: Double -> IO (F.ForeignPtr Word8, Int)
storeDoubleShortest d
 = case DC.toShortest d of
        BS.PS p _ n  -> return (p, n)
{-# INLINE storeDoubleShortest #-}


-- | Like `showDoubleShortest`, but use a fixed number of digits after
--   the decimal point.
storeDoubleFixed :: Int -> Double -> IO (F.ForeignPtr Word8, Int)
storeDoubleFixed !prec !d
 = case DC.toFixed prec d of
        BS.PS p _ n  -> return (p, n)
{-# INLINE storeDoubleFixed #-}

