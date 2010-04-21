{-# LANGUAGE BangPatterns #-}

-- | Solver for the Laplace equation
--	Writes a PPM file of the completed solution.
--
--	You can use the ImageMagick convert program to make a png
--	with	"convert out.ppm out.png"
--
import Solver
import Data.Array.Repa		as A
import ColorRamp
import PPM
import System.Environment

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [steps, fileInput, fileOutput]	
	    -> laplace (read steps) fileInput fileOutput

	  _ -> do
		putStr usage
		return ()


-- | Command line usage information.
usage	:: String
usage	= unlines
	[ "Usage: laplace <iterations> <input.ppm> <output.ppm>"
	, ""
	, "  iterations  :: Int       Number of iterations to use in the solver."
	, "  input.ppm   :: FileName  ASCII 8 bit RGB PPM file for initial and boundary values."
	, "  output.ppm  :: FileName  PPM file to write output to."
	, "" 
	, "  Format of input file:"
	, "      Boundary values are indicated in greyscale,"
	, "        ie from the list [(x, x, x) | x <- [0 .. 255]]"
	, "      Non-boundary values are indicated in blue,"
	, "        ie (0, 0, 255)"
	, "      Any other pixel value is an error." 
	, ""
	]
			

-- | Solve it.
laplace :: Int			-- ^ Number of iterations to use.
	-> FilePath 		-- ^ Input file.
	-> FilePath		-- ^ Output file
	-> IO ()

laplace steps fileInput fileOutput
 = do
	-- Load up the file containing boundary conditions.
	(matBoundMask, matBoundValue)	
		<- readPPMAsMatrix2 loadPixel fileInput

	-- Use the boundary condition values as the initial matrix.
	let matInitial	= matBoundValue

	-- Run the solver.
	let matFinal	= solveLaplace
				steps
				matBoundMask
				matBoundValue
				matInitial

	matFinal `deepSeqArray` return ()

	-- Write out the matrix as a colorised PPM image	
	writeMatrixAsNormalisedPPM
		fileOutput
		(rampColorHotToCold 0.0 1.0)
		matFinal


-- | Extract boundary mask and value from a pixel value.
loadPixel :: Int -> Int -> Int -> (Double, Double)
loadPixel r g b
	-- A non-boundary value.
 	| r == 0 && g == 0 && b == 255	
	= (1, 0)

	-- A boundary value.
	| (r == g) && (r == b) 
	= (0, (fromIntegral r) / 255)
	
	| otherwise
	= error $ "Unhandled pixel value in input " ++ show (r, g, b)
	
	
