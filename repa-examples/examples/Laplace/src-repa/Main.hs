{-# LANGUAGE BangPatterns #-}

-- | Solver for the Laplace equation
--	You supply a BMP files specifying the boundary conditions.
--	The output is written back to another BMP file.
--
import SolverGet		as SG
import SolverStencil		as SS
import Data.Array.Repa		as A
import Data.Array.Repa.IO.BMP	
import Data.Array.Repa.IO.ColorRamp
import Data.Array.Repa.IO.Timing
import System.Environment
import Data.Word
import Control.Monad
import Prelude 			as P

type RGB8
        = (Word8, Word8, Word8)

type Solver 
	=  Int			-- ^ Number of iterations to use.
	-> Array U DIM2 Double	-- ^ Boundary value mask.
	-> Array U DIM2 Double	-- ^ Boundary values.
	-> Array U DIM2 Double	-- ^ Initial state.
	-> Array U DIM2 Double

solvers 
 = 	[ ("get", 	SG.solveLaplace)
	, ("stencil", 	SS.solveLaplace) ]


main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [strSolver, steps, fileInput, fileOutput]	
	    -> 	let Just solver	= lookup strSolver solvers
	  	in  laplace solver (read steps) fileInput fileOutput

	  _ -> do
		putStr usage
		return ()


-- | Command line usage information.
usage	:: String
usage	= unlines
	[ "Usage: laplace <solver> <iterations> <input.bmp> <output.bmp>"
	, ""
	, "  iterations  :: Int       Number of iterations to use in the solver."
	, "  input.bmp   :: FileName  Uncompressed RGB24 or RGBA32 BMP file for initial and boundary values."
	, "  output.bmp  :: FileName  BMP file to write output to."
	, "" 
	, "  solver = one of " P.++ show (P.map fst solvers)
	, ""
	, "  Format of input file:"
	, "      Boundary values are indicated in greyscale,"
	, "        ie from the list [(x, x, x) | x <- [0 .. 255]]"
	, "      Non-boundary values are indicated in blue,"
	, "        ie (0, 0, 255)"
	, "      Any other pixel value is an error." 
	, ""
	, "  NOTE: For GHC 7.0.3, this runs better when you turn off the parallel"
	, "        garbage collector. Run with +RTS -qg"
	, "" ]
			

-- | Solve it.
laplace :: Solver
	-> Int			-- ^ Number of iterations to use.
	-> FilePath 		-- ^ Input file.
	-> FilePath		-- ^ Output file
	-> IO ()

laplace solve steps fileInput fileOutput
 = do
	-- Load up the file containing boundary conditions.
	arrImage		<- liftM (either (error . show) id)
				$  readImageFromBMP fileInput
	arrImage `deepSeqArray` return ()

	let arrBoundValue	= compute $ A.map slurpBoundValue arrImage
	arrBoundValue `deepSeqArray` return ()

	let arrBoundMask	= compute $ A.map slurpBoundMask  arrImage
	arrBoundMask  `deepSeqArray` return ()

	let arrInitial		= arrBoundValue		
	
	-- Run the solver.
	(arrFinal, t)
		<- time
		$  let	arrFinal = solve steps arrBoundMask arrBoundValue arrInitial
		   in	arrFinal `deepSeqArray` return arrFinal

	-- Print how long it took
	putStr (prettyTime t)

	-- Make the result image
	let arrImageOut         = compute $ A.map makePixel arrFinal

	-- Write out the image to file.	
	writeImageToBMP
		fileOutput
		arrImageOut


-- | Extract the boundary value from a RGB triple.
slurpBoundValue :: RGB8 -> Double
{-# INLINE slurpBoundValue #-}
slurpBoundValue (!r, !g, !b)
	-- A non-boundary value.
 	| r == 0 && g == 0 && b == 255	
	= 0

	-- A boundary value.
	| (r == g) && (r == b) 
	= fromIntegral (fromIntegral r :: Int) / 255
	
	| otherwise
	= error $ "Unhandled pixel value in input " P.++ show (r, g, b)


-- | Extract boundary mask from a RGB triple.
slurpBoundMask :: RGB8 -> Double
{-# INLINE slurpBoundMask #-}
slurpBoundMask (!r, !g, !b)
	-- A non-boundary value.
 	| r == 0 && g == 0 && b == 255	
	= 1

	-- A boundary value.
	| (r == g) && (r == b) 
	= 0
	
	| otherwise
	= error $ "Unhandled pixel value in input " P.++ show (r, g, b)


-- | Make an RGB pixel from a final array value.
makePixel :: Double -> RGB8
{-# INLINE makePixel #-}
makePixel d
 = let  (rd, gd, bd)       = rampColorHotToCold 0.0 1.0 d
   in   ( truncate (rd * 255)
        , truncate (gd * 255)
        , truncate (bd * 255))
        