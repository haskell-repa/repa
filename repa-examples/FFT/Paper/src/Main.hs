{-# LANGUAGE PatternGuards #-}

-- | Perform high pass filtering on a BMP image.
import Data.Array.Repa.Algorithms.DFT.Center
import Data.Array.Repa.Algorithms.DFT.Roots
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa.IO.BMP
import Data.Array.Repa				as A
import System.Environment
import Control.Monad
import Paper

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	 [cutoff, fileIn, fileOut]
	   -> mainWithArgs (read cutoff) fileIn fileOut

         _ -> putStr $ unlines
		[ "Usage: repa-fft-highpass <cutoff::Int> <fileIn.bmp> <fileOut.bmp>"
		, "" ]
			
	
mainWithArgs cutoff fileIn fileOut
 = do	
	-- Load in the matrix.
	(arrRed, arrGreen, arrBlue)
		<- liftM (either (\e -> error $ show e) id)
		$  readComponentsFromBMP fileIn
		
	-- Do the transform on each component individually
	let arrRed'	= transform cutoff arrRed
	let arrGreen'	= transform cutoff arrGreen
	let arrBlue'	= transform cutoff arrBlue
	
	-- Write it back to file.
	writeComponentsToBMP fileOut
		arrRed' arrGreen' arrBlue' 
		

transform cutoff arrReal
 = let	arrComplex	= force $ A.map (\r -> (fromIntegral r) :*: 0) arrReal
			
	-- Do the 2d transform.
	arrCentered	= center2d arrComplex
	rofuForward	= calcRootsOfUnity (extent arrComplex)
--	arrFreq		= fft2d rofuForward arrCentered
	arrFreq		= fft2d rofuForward arrCentered


	-- Zap out the low frequency components.
	_ :. height :. width = extent arrFreq
	centerX		= width  `div` 2
	centerY		= height `div` 2
	
	{-# INLINE highpass #-}
	highpass get ix@(_ :. y :. x)
		|   x > centerX + cutoff
		 || x < centerX - cutoff
		 || y > centerY + cutoff
		 || y < centerY - cutoff
		= get ix
		
		| otherwise
		= 0
		
	arrFilt	= traverse arrFreq id highpass

	-- Do the inverse transform to get back to image space.
	rofuReverse	= calcInverseRootsOfUnity (extent arrFilt)
	arrReverse	= fft2d rofuReverse arrFilt

	scale		= fromIntegral (height * width)
	arrInv		= A.map (/ scale) arrReverse
		
	-- Get the magnitude of the transformed array, 
	arrMag	= A.map (truncate . mag) arrInv

   in	arrMag

