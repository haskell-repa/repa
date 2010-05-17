
-- | Perform the 2D FFT on a BMP image.
import Data.Array.Repa.Algorithms.FFT
import Data.Array.Repa.Algorithms.DFT.Center
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa.IO.BMP
import Data.Array.Repa				as A
import System.Environment
import Control.Monad

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	 [fileIn, clipMag, fileMag, filePhase]
	   -> mainWithArgs fileIn (read clipMag) fileMag filePhase

         _ -> putStr $ unlines
		[ "Usage: repa-fft2d <fileIn.bmp> <clip mag :: Int> <fileOutMag.bmp> <fileOutPhase.bmp>"
		, ""
		, "    The output magnitude has a high dynamic range. We need to clip it otherwise"
		, "    most of the pixels in the output BMP will be black. Start with a value equal"
		, "    to about the width of the image (eg 512)"
		, "" ]
			
	
mainWithArgs fileIn clipMag fileMag filePhase
 = do	
	-- Load in the matrix.
	arrReal		<- liftM (either (\e -> error $ show e) id)
			$  readMatrixFromGreyscaleBMP fileIn
	let arrComplex	= force $ A.map (\r -> r :*: 0) arrReal
	
	-- Apply the centering transform so that the output has the zero
	--	frequency in the middle of the image.
	let arrCentered	= centerMatrix arrComplex
		
	-- Do the 2d transform.
	let arrFreq	= fft2d Forward arrCentered
		
	-- Write out the magnitude of the transformed array, 
	--	clipping it at the given value.
	let clip m	= if m >= clipMag then clipMag else m
	let arrMag	= A.map (clip . mag) arrFreq
	writeMatrixToGreyscaleBMP fileMag arrMag

	-- Write out the phase of the transofmed array, 
	-- 	scaling it to make full use of the 8 bit greyscale.
	let scaledArg x	= (arg x + pi) * (255 / (2 * pi))
	let arrPhase	= A.map scaledArg arrFreq
	writeMatrixToGreyscaleBMP filePhase arrPhase

