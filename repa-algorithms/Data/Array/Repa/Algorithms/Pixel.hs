
-- | Utilities for converting pixel color values.
--
-- NOTE: These functions are not polymorphic in the Float type because
--       without assisatance, GHC does a bad job of converting Word8s 
--       to and from floats. 
--
module Data.Array.Repa.Algorithms.Pixel
        ( floatRmsOfRGB8
        , doubleRmsOfRGB8
        , floatLuminanceOfRGB8
        , doubleLuminanceOfRGB8
        , rgb8OfGreyFloat
        , rgb8OfGreyDouble
        , rgb8OfFloat
        , rgb8OfDouble)
where
import Data.Word


-- | Compute the root mean square of an RGB color. Result is in the range [0..1].
floatRmsOfRGB8 :: (Word8, Word8, Word8) -> Float
{-# INLINE floatRmsOfRGB8 #-}
floatRmsOfRGB8 (r, g, b)
 = let  r'      = fromIntegral (fromIntegral r :: Int) / 255
        g'      = fromIntegral (fromIntegral g :: Int) / 255
        b'      = fromIntegral (fromIntegral b :: Int) / 255
        s       = ((r' * r') + (g' * g') + (b' * b')) / 3
   in   sqrt s


-- | Compute the root mean square of an RGB color. Result is in the range [0..1].
doubleRmsOfRGB8 :: (Word8, Word8, Word8) -> Float
{-# INLINE doubleRmsOfRGB8 #-}
doubleRmsOfRGB8 (r, g, b)
 = let  r'      = fromIntegral (fromIntegral r :: Int) / 255
        g'      = fromIntegral (fromIntegral g :: Int) / 255
        b'      = fromIntegral (fromIntegral b :: Int) / 255
        s       = ((r' * r') + (g' * g') + (b' * b')) / 3
   in   sqrt s


-- | Convert an RGB color to its luminance value. Result in the range [0..1].
floatLuminanceOfRGB8 :: (Word8, Word8, Word8) -> Float
{-# INLINE floatLuminanceOfRGB8 #-}
floatLuminanceOfRGB8 (r, g, b)
 = let  r'      = fromIntegral (fromIntegral r :: Int) / 255
        g'      = fromIntegral (fromIntegral g :: Int) / 255
        b'      = fromIntegral (fromIntegral b :: Int) / 255
   in   r' * 0.3 + g' * 0.59 + b' * 0.11


-- | Convert an RGB color to its luminance value. Result in the range [0..1].
doubleLuminanceOfRGB8 :: (Word8, Word8, Word8) -> Double
{-# INLINE doubleLuminanceOfRGB8 #-}
doubleLuminanceOfRGB8 (r, g, b)
 = let  r'      = fromIntegral (fromIntegral r :: Int) / 255
        g'      = fromIntegral (fromIntegral g :: Int) / 255
        b'      = fromIntegral (fromIntegral b :: Int) / 255
   in   r' * 0.3 + g' * 0.59 + b' * 0.11


-- | Promote a value in the range [0..1] to a grey RGB8 color.
rgb8OfGreyFloat :: Float -> (Word8, Word8, Word8)
{-# INLINE rgb8OfGreyFloat #-}
rgb8OfGreyFloat x
 = let  v        = fromIntegral (truncate (x * 255) :: Int)
   in   (v, v, v)


-- | Promote a value in the range [0..1] to a grey RGB8 color.
rgb8OfGreyDouble :: Double -> (Word8, Word8, Word8)
{-# INLINE rgb8OfGreyDouble #-}
rgb8OfGreyDouble x
 = let  v        = fromIntegral (truncate (x * 255) :: Int)
   in   (v, v, v)


-- | Promote a tuple of color components to a RGB8 color. 
--   Each of the source components should be in the range [0..1].
rgb8OfFloat :: (Float, Float, Float) -> (Word8, Word8, Word8)
{-# INLINE rgb8OfFloat #-}
rgb8OfFloat (r, g, b)
 = let  r8      = fromIntegral (truncate (r * 255) :: Int)
        g8      = fromIntegral (truncate (g * 255) :: Int)
        b8      = fromIntegral (truncate (b * 255) :: Int)
   in   (r8, g8, b8)


-- | Promote a tuple of color components to a RGB8 color. 
--   Each of the source components should be in the range [0..1].
rgb8OfDouble :: (Double, Double, Double) -> (Word8, Word8, Word8)
{-# INLINE rgb8OfDouble #-}
rgb8OfDouble (r, g, b)
 = let  r8      = fromIntegral (truncate (r * 255) :: Int)
        g8      = fromIntegral (truncate (g * 255) :: Int)
        b8      = fromIntegral (truncate (b * 255) :: Int)
   in   (r8, g8, b8)
