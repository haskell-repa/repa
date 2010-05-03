
-- | Perform a 2D transform on a BMP image file.
import Data.Array.Repa.Algorithms.FFT
import Data.Array.Repa.Algorithms.DFT.Center
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa				as A
import System.Environment
import Control.Monad
import BMP

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	 [fileIn, clipMag, fileMag, filePhase]
	   -> mainWithArgs fileIn (read clipMag) fileMag filePhase

	
mainWithArgs fileIn clipMag fileMag filePhase
 = do	
	-- Load in the matrix
	arrReal		<- readMatrixFromBMP fileIn
	let arrComplex	= force $ A.map (\r -> r :*: 0) arrReal
	
	-- Apply the centering transform so that the output has the zero
	--	frequency in the middle of the plot.
	let arrCentered	= centerMatrix arrComplex
		
	-- Do the 2d transform.
	let arrFreq	= fft2d arrCentered
		
	-- Compute the magnitude of the transformed array.
	let clip m	= if m >= clipMag then clipMag else m
	let arrMag	= A.map (clip . mag) arrFreq
	
	-- Write the output back to files.
	writeMatrixToGreyscaleBMP fileMag arrMag
