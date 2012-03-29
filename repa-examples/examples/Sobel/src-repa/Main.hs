{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

-- | Apply Sobel operators to an image.
import System.Environment
import Data.Array.Repa 			        as R
import Data.Array.Repa.Algorithms.Pixel         as R
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing
import Prelude				hiding (compare)
import Control.Monad
import Solver
import Debug.Trace

-- Main routine ---------------------------------------------------------------
main 
 = do	args	<- getArgs
	case args of
	 [iterations, fileIn, fileOut]	
		-> run (read iterations) fileIn fileOut
	 _	-> putStrLn "Usage: sobel <iterations::Int> <fileIn.bmp> <fileOut.bmp>"



run iterations fileIn fileOut
 = do	-- Load the source image and convert it to greyscale
        inputImage 	<- liftM (either (error . show) id) 
			$ readImageFromBMP fileIn

        (greyImage :: Array U DIM2 Float)
                        <- now $ computeP
                        $  R.map luminanceOfRGB8 inputImage
		
        -- Run the filter.
	((gX, gY), tElapsed)
	               <- time $ loop iterations greyImage

	putStr $ prettyTime tElapsed
	
	-- Write out the magnitute of the vector gradient as the result image.
	outImage       <- now $ computeUnboxedP
	               $  R.map rgb8OfGrey  
	               $  R.zipWith magnitude gX gY	

	writeImageToBMP fileOut outImage


loop :: Int -> Image -> IO (Image, Image)
loop n img
 = img `deepSeqArray`
   if n == 0
    then return (img, img)
    else do 
	gX      <- now $ gradientX img
	gY	<- now $ gradientY img	
	if (n == 1) 
		then return (gX, gY)
		else loop (n - 1) img


-- | Determine the squared magnitude of a vector.
magnitude :: Float -> Float -> Float
{-# INLINE magnitude #-}
magnitude x y
        = sqrt (x * x + y * y)

