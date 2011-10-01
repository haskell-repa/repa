
-- | Utilities for converting pixel color values.
module Data.Array.Repa.Algorithms.Pixel
        ( RGB8
        , rmsOfRGB8
        , luminanceOfRGB8
        , rgb8OfGrey
        , rgb8OfFrac)
where
import Data.Word

type RGB8       = (Word8, Word8, Word8)


-- | Compute the root mean square of an RGB color. Result is in the range [0..1].
rmsOfRGB8 :: Floating a => (Word8, Word8, Word8) -> a
{-# INLINE rmsOfRGB8 #-}
rmsOfRGB8 (r, g, b)
 = let  r'      = fromIntegral r / 255
        g'      = fromIntegral g / 255
        b'      = fromIntegral b / 255
        s       = ((r' * r') + (g' * g') + (b' * b')) / 3
   in   sqrt s


-- | Convert an RGB color to its luminance value. Result in the range [0..1].
luminanceOfRGB8 :: Floating a => RGB8 -> a
{-# INLINE luminanceOfRGB8 #-}
luminanceOfRGB8 (r, g, b)
 = let  r'      = fromIntegral r / 255
        g'      = fromIntegral g / 255
        b'      = fromIntegral b / 255
   in   r' * 0.3 + g' * 0.59 + b' * 0.11


-- | Promote a value in the range [0..1] to a grey RGB8 color.
rgb8OfGrey :: RealFrac a => a -> RGB8
{-# INLINE rgb8OfGrey #-}
rgb8OfGrey x
 = let  v        = truncate (x * 255)
   in   (v, v, v)


-- | Promote a tuple of color components to a RGB8 color. 
--   Each of the source components should be in the range [0..1].
rgb8OfFrac :: RealFrac a => (a, a, a) -> RGB8
{-# INLINE rgb8OfFrac #-}
rgb8OfFrac (r, g, b)
 = let  r8      = truncate (r * 255)
        g8      = truncate (g * 255)
        b8      = truncate (b * 255)
   in   (r8, g8, b8)

