
-- | Utilities for converting pixel color values.
module Data.Array.Repa.Algorithms.Pixel
        ( rmsOfRGB8
        , luminanceOfRGB8
        , rgb8OfGrey
        , rgb8OfFrac)
where
import Data.Word

-- NOTE: These functions are not polymorphic in the Float type because
--       without assisatance, GHC does a very bad job of converting Word8s 
--       to and from floats. This is also the reason we do a manual covnersion
--       to the Int type.

-- | Compute the root mean square of an RGB color. Result is in the range [0..1].
rmsOfRGB8 :: (Word8, Word8, Word8) -> Float
{-# INLINE rmsOfRGB8 #-}
rmsOfRGB8 (r, g, b)
 = let  r'      = fromIntegral (fromIntegral r :: Int) / 255
        g'      = fromIntegral (fromIntegral g :: Int) / 255
        b'      = fromIntegral (fromIntegral b :: Int) / 255
        s       = ((r' * r') + (g' * g') + (b' * b')) / 3
   in   sqrt s


-- | Convert an RGB color to its luminance value. Result in the range [0..1].
luminanceOfRGB8 :: (Word8, Word8, Word8) -> Float
{-# INLINE luminanceOfRGB8 #-}
luminanceOfRGB8 (r, g, b)
 = let  r'      = fromIntegral (fromIntegral r :: Int) / 255
        g'      = fromIntegral (fromIntegral g :: Int) / 255
        b'      = fromIntegral (fromIntegral b :: Int) / 255
   in   r' * 0.3 + g' * 0.59 + b' * 0.11


-- | Promote a value in the range [0..1] to a grey RGB8 color.
rgb8OfGrey :: Float -> (Word8, Word8, Word8)
{-# INLINE rgb8OfGrey #-}
rgb8OfGrey x
 = let  v        = fromIntegral (truncate (x * 255) :: Int)
   in   (v, v, v)


-- | Promote a tuple of color components to a RGB8 color. 
--   Each of the source components should be in the range [0..1].
rgb8OfFrac :: (Float, Float, Float) -> (Word8, Word8, Word8)
{-# INLINE rgb8OfFrac #-}
rgb8OfFrac (r, g, b)
 = let  r8      = fromIntegral (truncate (r * 255) :: Int)
        g8      = fromIntegral (truncate (g * 255) :: Int)
        b8      = fromIntegral (truncate (b * 255) :: Int)
   in   (r8, g8, b8)

