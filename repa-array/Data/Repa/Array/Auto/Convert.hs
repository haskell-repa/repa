
module Data.Repa.Array.Auto.Convert
        ( -- * Int Conversion
          readIntFromOffset,    readIntFromOffset#

          -- * Double Conversion
          -- | Read and Show `Double`s for a reasonable runtime cost.
        , readDouble,           readDoubleFromBytes
        , showDouble,           showDoubleAsBytes
        , showDoubleFixed,      showDoubleFixedAsBytes)
where
import Data.Repa.Array.Auto.Base
import Data.Repa.Array.Generic.Convert
import System.IO.Unsafe
import Data.Word
import Data.Char
import GHC.Ptr
import GHC.Exts

import qualified Data.Repa.Array.Material.Auto          as A
import qualified Data.Repa.Array.Material.Foreign       as A
import qualified Data.Repa.Array.Meta                   as A
import qualified Data.Repa.Array.Generic                as A

import qualified Data.Double.Conversion.ByteString      as DC

import qualified Foreign.ForeignPtr                     as F
import qualified Foreign.Storable                       as F
import qualified Foreign.Marshal.Alloc                  as F
import qualified Foreign.Marshal.Utils                  as F


-------------------------------------------------------------------------------
-- | Try to read an `Int` from the given offset in an array.
-- 
--   If the conversion succeeded then you get the value, 
--   along with the index of the next character, 
--   otherwise `Nothing`.
--
readIntFromOffset  :: Array Char -> Int -> Maybe (Int, Int)
readIntFromOffset arr (I# ix0)
 = case readIntFromOffset# arr ix0 of
        (# 0#, _, _  #)  -> Nothing
        (# _ , n, ix #)  -> Just (I# n, I# ix)
{-# INLINE readIntFromOffset #-}


-- | Unboxed version of `readIntFromOffset`.
--
--   We still pay to unbox the input array, 
--   but avoid boxing the result by construction.
--
readIntFromOffset# :: Array Char -> Int# -> (# Int#, Int#, Int# #)
readIntFromOffset# !arr !ix0_
 = start ix0
 where

        !ix0    = I# ix0_
        !len    = A.length arr

        start !ix
         | ix >= len    = (# 0#, 0#, 0# #)
         | otherwise    = sign ix

        -- Check for explicit sign character,
        -- and encode what it was as an integer.
        sign !ix
         | !s   <- A.index arr 0
         = case s of
                '-'     -> loop 1 (ix + 1) 0
                '+'     -> loop 2 (ix + 1) 0
                _       -> loop 0  ix      0

        loop !(neg :: Int) !ix !n 
         -- We've hit the end of the array.
         | ix >= len   
         = end neg ix n

         | otherwise
         = case ord $ A.index arr ix of
               -- Current character is a digit, so add it to the accmulator.
             w |  w >= 0x30 && w <= 0x039
               -> loop neg (ix + 1) (n * 10 + (fromIntegral w - 0x30))

               -- Current character is not a digit.
               | otherwise
               -> end neg ix n

        end !neg !ix !n
         -- We didn't find any digits, and there was no explicit sign.
         | ix  == ix0
         , neg == 0  
         = (# 0#, 0#, 0# #)

         -- We didn't find any digits, but there was an explicit sign.
         | ix  == (ix0 + 1)
         , neg /= 0  
         = (# 0#, 0#, 0# #)

         -- Number was explicitly negated.
         | neg == 1                    
         , I# n'        <- negate n
         , I# ix'       <- ix
         = (# 1#, n', ix' #)

         -- Number was not negated.
         | otherwise
         , I# n'        <- n
         , I# ix'       <- ix
         = (# 1#, n', ix' #)
{-# NOINLINE readIntFromOffset# #-}


-------------------------------------------------------------------------------
-- | Convert a foreign vector of characters to a Double.
--
--   * The standard Haskell `Char` type is four bytes in length.
--     If you already have a vector of `Word8` then use `readDoubleFromBytes`
--     instead to avoid the conversion.
--
readDouble :: Array Char -> Double
readDouble vec
        = readDoubleFromBytes
        $ A.computeS A.A $ A.map (fromIntegral . ord) vec
{-# INLINE readDouble #-}


-- | Convert a foreign vector of bytes to a Double.
readDoubleFromBytes :: Array Word8 -> Double
readDoubleFromBytes 
   (A.toForeignPtr . convert -> (start, len, fptr :: F.ForeignPtr Word8))
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


-------------------------------------------------------------------------------
-- | Convert a `Double` to ASCII text packed into a foreign `Vector`.
showDouble :: Double -> Array Char
showDouble !d
        = A.computeS A.A $ A.map (chr . fromIntegral)
        $ showDoubleAsBytes d
{-# INLINE showDouble #-}


-- | Convert a `Double` to ASCII text packed into a foreign `Vector`.
showDoubleAsBytes :: Double -> Array Word8
showDoubleAsBytes !d
        = convert
        $ A.fromByteString $ DC.toShortest d
{-# INLINE showDoubleAsBytes #-}


-- | Like `showDouble`, but use a fixed number of digits after
--   the decimal point.
showDoubleFixed :: Int -> Double -> Array Char
showDoubleFixed !prec !d
        = A.computeS A.A $ A.map (chr . fromIntegral)
        $ showDoubleFixedAsBytes prec d
{-# INLINE showDoubleFixed #-}


-- | Like `showDoubleAsBytes`, but use a fixed number of digits after
--   the decimal point.
showDoubleFixedAsBytes :: Int -> Double -> Array Word8
showDoubleFixedAsBytes !prec !d
        = convert
        $ A.fromByteString $ DC.toFixed prec d
{-# INLINE showDoubleFixedAsBytes #-}

