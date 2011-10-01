{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

-- | Apply Sobel operators to an image.
import Data.Word
import Control.Monad
import System.Environment
import Data.Array.Repa 			        as R
import qualified Data.Array.Repa.Repr.Unboxed   as U
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
	 _	-> putStrLn "Usage: sobel <iterations::Int> <fileIn.bmp> <fileOut.bmp>"


run iterations fileIn fileOut
 = do	inputImage 	<- liftM (compute . either (error . show) id) 
			$ readImageFromBMP fileIn
	
	let greyImage	= toGreyScale inputImage
	greyImage `deepSeqArray` return ()
		
	(result, tElapsed)
		<- time $ let 	(gX, gY)	= loop iterations greyImage
			  in	gX `deepSeqArray` gY `deepSeqArray` return (gX, gY)

	putStr $ prettyTime tElapsed
	
	let (gX, gY)	= result
	let outImage	= compute $ R.zipWith magnitude gX gY	

	outImage `seq` return ()
	writeImageToBMP fileOut 
	        (U.zip3 outImage outImage outImage)


loop :: Int -> Image -> (Image, Image)
loop n img
 = img `deepSeqArray`
   if n == 0
    then (img, img)
    else do 
	let gX	= gradientX img
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
toGreyScale :: Array U DIM3 Word8 -> Image
{-# NOINLINE toGreyScale #-}
toGreyScale arr
  = arr `deepSeqArray`
   compute $ traverse arr
	(\(sh :. _) -> sh)
	(\get ix    -> rgbToLuminance 
				(get (ix :. 0))
				(get (ix :. 1))
				(get (ix :. 2)))


** shift pixel conversions, this and greyscale to algorithms 
** also shift colorRamp to algorithms

-- | Convert a RGB value to a luminance.
rgbToLuminance :: Word8 -> Word8 -> Word8 -> Float
{-# INLINE rgbToLuminance #-}
rgbToLuminance r g b 
	= fromIntegral r * 0.3
	+ fromIntegral g * 0.59
	+ fromIntegral b * 0.11
