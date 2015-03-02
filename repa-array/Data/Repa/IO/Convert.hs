{-# LANGUAGE ViewPatterns #-}
module Data.Repa.IO.Convert
        ( -- * Conversion
          -- | Read and Show `Double`s for a reasonable runtime cost.
          readDouble,           readDoubleFromBytes
        , showDouble,           showDoubleAsBytes
        , showDoubleFixed,      showDoubleFixedAsBytes

          -- * Date conversion
        , packDate32,           unpackDate32
        , readYYYYsMMsDD)
where
import Data.Repa.Array.Material.Foreign                 as A
import Data.Repa.Array.Material.Unboxed                 as A
import Data.Repa.Array                                  as A
import System.IO.Unsafe
import Data.Word
import Data.Bits
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
readDouble :: Array F Char -> Double
readDouble vec
        = readDoubleFromBytes
        $ A.computeS F $ A.map (fromIntegral . ord) vec
{-# INLINE readDouble #-}


-- | Convert a foreign vector of bytes to a Double.
readDoubleFromBytes :: Array F Word8 -> Double
readDoubleFromBytes (toForeignPtr -> (start,len,fptr))
 = unsafePerformIO
 $ F.allocaBytes (len + 1) $ \pBuf ->
   F.alloca                $ \pRes ->
   F.withForeignPtr fptr   $ \pIn  ->
    do
        -- Copy the data to our new buffer.
        F.copyBytes   pBuf (pIn `plusPtr` start) (fromIntegral len)

        -- Poke a 0 on the end to ensure it's null terminated.
        F.pokeByteOff pBuf len (0 :: Word8)

        -- Call the C strtod function
        let !d  = strtod pBuf pRes

        return d
{-# NOINLINE readDoubleFromBytes #-}

foreign import ccall unsafe
 strtod :: Ptr Word8 -> Ptr (Ptr Word8) -> Double


-- | Convert a `Double` to ASCII text packed into a foreign `Vector`.
showDouble :: Double -> Array F Char
showDouble !d
        = A.computeS F $ A.map (chr . fromIntegral)
        $ showDoubleAsBytes d
{-# INLINE showDouble #-}


-- | Convert a `Double` to ASCII text packed into a foreign `Vector`.
showDoubleAsBytes :: Double -> Array F Word8
showDoubleAsBytes !d
        = fromByteString $ DC.toShortest d
{-# INLINE showDoubleAsBytes #-}


-- | Like `showDouble`, but use a fixed number of digits after
--   the decimal point.
showDoubleFixed :: Int -> Double -> Array F Char
showDoubleFixed !prec !d
        = A.computeS F $ A.map (chr . fromIntegral)
        $ showDoubleFixedAsBytes prec d
{-# INLINE showDoubleFixed #-}


-- | Like `showDoubleAsBytes`, but use a fixed number of digits after
--   the decimal point.
showDoubleFixedAsBytes :: Int -> Double -> Array F Word8
showDoubleFixedAsBytes !prec !d
        = fromByteString $ DC.toFixed prec d
{-# INLINE showDoubleFixedAsBytes #-}


-- Dates ----------------------------------------------------------------------
-- | Pack a year, month and day into a `Word32`.
--
--   Bitwise format is:
--   
-- @
-- 32             16       8      0 
-- | year          | month | day  |
-- @
--
--   If they components of the date are out-of-range then they will be bit-wise
--   truncate so they fit in their destination fields.
--  
packDate32   :: (Word, Word, Word) -> Word32
packDate32 (yy, mm, dd) 
        =   ((fromIntegral yy .&. 0x0ffff) `shiftL` 16) 
        .|. ((fromIntegral mm .&. 0x0ff)   `shiftL` 8)
        .|.  (fromIntegral dd .&. 0x0ff)
{-# INLINE packDate32 #-}


-- | Inverse of `packDate32`.
--
--   This function does a simple bit-wise unpacking of the given `Word32`, 
--   and does not guarantee that the returned fields are within a valid 
--   range for the given calendar date.
--
unpackDate32  :: Word32 -> (Word, Word, Word)
unpackDate32 date
        = ( fromIntegral $ (date `shiftR` 16) .&. 0x0ffff
          , fromIntegral $ (date `shiftR` 8)  .&. 0x0ff
          , fromIntegral $ date               .&. 0x0ff)
{-# INLINE unpackDate32 #-}


-- | Read a date in ASCII YYYYsMMsDD format, using the given separator
--   character 's'.
--
--   TODO: avoid going via lists.
--
readYYYYsMMsDD 
        :: BulkI l Char
        => Char -> Array l Char -> Maybe (Word, Word, Word)
readYYYYsMMsDD sep arr
 = case words 
        $ A.toList
        $ A.mapS U (\c -> if c == sep then ' ' else c) arr of
                [yy, mm, dd]    -> Just (read yy, read mm, read dd)
                _               -> Nothing
{-# INLINE readYYYYsMMsDD #-}
