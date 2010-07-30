{-# LANGUAGE BangPatterns #-}

-- | Solver for the Laplace equation
--	You supply a BMP files specifying the boundary conditions.
--	The output is written back to another BMP file.
--
import Solver
import Data.Array.Repa		as A
import Data.Array.Repa.IO.BMP	
import Data.Array.Repa.IO.ColorRamp
import Data.Array.Repa.IO.Timing
import System.Environment
import Data.Word
import Control.Monad

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
	[ "Usage: laplace <iterations> <input.bmp> <output.bmp>"
	, ""
	, "  iterations  :: Int       Number of iterations to use in the solver."
	, "  input.bmp   :: FileName  Uncompressed RGB24 or RGBA32 BMP file for initial and boundary values."
	, "  output.bmp  :: FileName  BMP file to write output to."
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
	arrImage		<- liftM (either (error . show) id)
				$  readImageFromBMP fileInput

	let arrBoundValue	= force $ slurpDoublesFromImage slurpBoundValue arrImage
	let arrBoundMask	= force $ slurpDoublesFromImage slurpBoundMask  arrImage
		
	-- Use the boundary condition values as the initial matrix.
	let arrInitial	= arrBoundValue

	arrBoundValue 
	 `deepSeqArray` arrBoundMask
	 `deepSeqArray` arrInitial
	 `deepSeqArray` return ()

	-- Run the solver.
	(arrFinal, t)
		<- time
		$  let arrFinal	= solveLaplace
					steps
					arrBoundMask
					arrBoundValue
					arrInitial
		    in	arrFinal `deepSeqArray` return arrFinal

	-- Print how long it took
	putStr (prettyTime t)

	-- Make the result image
	let arrImageOut		
		= makeImageFromDoubles (rampColorHotToCold 0.0 1.0) arrFinal

	-- Write out the image to file.	
	writeImageToBMP
		fileOutput
		arrImageOut



slurpDoublesFromImage
	:: (Word8 -> Word8 -> Word8 -> Double)
	-> Array DIM3 Word8
	-> Array DIM2 Double
	
{-# INLINE slurpDoublesFromImage #-}
slurpDoublesFromImage mkDouble arrBound
 = traverse arrBound
	(\(Z :. height :. width :. _)	
		-> Z :. height :. width)

	(\get (Z :. y :. x)
		-> mkDouble
			(get (Z :. y :. x :. 0))
			(get (Z :. y :. x :. 1))
			(get (Z :. y :. x :. 2)))


makeImageFromDoubles
	:: (Double -> (Double, Double, Double))
	-> Array DIM2 Double
	-> Array DIM3 Word8
	
{-# INLINE makeImageFromDoubles #-}
makeImageFromDoubles fnColor arrDoubles
 = traverse arrDoubles
	(\(Z :. height :. width)
		-> Z :. height :. width :. 4)
		
	(\get (Z :. y :. x :. c)
		-> let (r, g, b) = fnColor (get (Z :. y :. x))
		   in	case c of
			  0	-> truncate (r * 255)
			  1	-> truncate (g * 255)
			  2	-> truncate (b * 255)
			  3	-> 0)


-- | Extract the boundary value from a RGB triple.
slurpBoundValue :: Word8 -> Word8 -> Word8 -> Double
{-# INLINE slurpBoundValue #-}
slurpBoundValue r g b
	-- A non-boundary value.
 	| r == 0 && g == 0 && b == 255	
	= 0

	-- A boundary value.
	| (r == g) && (r == b) 
	= fromIntegral r / 255
	
	| otherwise
	= error $ "Unhandled pixel value in input " ++ show (r, g, b)


-- | Extract boundary mask from a RGB triple.
slurpBoundMask :: Word8 -> Word8 -> Word8 -> Double
{-# INLINE slurpBoundMask #-}
slurpBoundMask r g b
	-- A non-boundary value.
 	| r == 0 && g == 0 && b == 255	
	= 1

	-- A boundary value.
	| (r == g) && (r == b) 
	= 0
	
	| otherwise
	= error $ "Unhandled pixel value in input " ++ show (r, g, b)
	
	
