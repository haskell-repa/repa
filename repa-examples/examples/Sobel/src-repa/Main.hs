{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

-- | Apply Sobel operators to an image.
import Data.Word
import Control.Monad
import System.Environment
import Data.Array.Repa 			as Repa
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing
import Prelude				hiding (compare)

import Solver


-- Main routine ---------------------------------------------------------------
main 
 = do	args	<- getArgs
	case args of
	 [iterations, fileIn, fileOut]	
		-> run (read iterations) fileIn fileOut
	 _	-> putStr "sobel <fileIn.bmp> <fileOut.bmp>"


run iterations fileIn fileOut
 = do	inputImage 	<- liftM (force . either (error . show) id) 
			$ readImageFromBMP fileIn
	
	let greyImage	= toGreyScale inputImage
	greyImage `deepSeqArray` return ()
	
	(result, tElapsed)
		<- time $ let 	(gX, gY)	= loop iterations greyImage
			  in	gX `deepSeqArray` gY `deepSeqArray` return (gX, gY)

	putStr $ prettyTime tElapsed
	
	let (gX, gY)	= result
	let outImage	= force $ Repa.zipWith magnitude gX gY	
	writeMatrixToGreyscaleBMP fileOut outImage


loop :: Int -> Image -> (Image, Image)

loop 0 !img@Manifest{}	
	= (img, img)

loop n !img@Manifest{}
 = do	let gX	= gradientX img
	let gY	= gradientY img

	if (n == 1) 
		then gX `deepSeqArray` gY `deepSeqArray` (gX, gY)
		else gX `deepSeqArray` gY `deepSeqArray` loop (n - 1) img


-- | Determine the squared magnitude of a vector.
magnitude :: Float -> Float -> Double
{-# INLINE magnitude #-}
magnitude x y
	= fromRational $ toRational $ sqrt (x * x + y * y)


-- | RGB to greyscale conversion.
toGreyScale :: Array DIM3 Word8 -> Image
{-# NOINLINE toGreyScale #-}
toGreyScale arr@Manifest{}
  = arr `deepSeqArray` force
  $ traverse arr
	(\(sh :. _) -> sh)
	(\get ix    -> rgbToLuminance 
				(get (ix :. 0))
				(get (ix :. 1))
				(get (ix :. 2)))


-- | Convert a RGB value to a luminance.
rgbToLuminance :: Word8 -> Word8 -> Word8 -> Float
{-# INLINE rgbToLuminance #-}
rgbToLuminance r g b 
	= fromIntegral r * 0.3
	+ fromIntegral g * 0.59
	+ fromIntegral b * 0.11
