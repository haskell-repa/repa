{-# LANGUAGE BangPatterns #-}

-- | Solver for the Laplace equation
--	Writes a PPM file of the completed solution.
--
--	You can use the ImageMagick convert program to make a png
--	with	"convert out.ppm out.png"
--
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

        matBoundMask
          `deepSeqArray` matBoundValue
          `deepSeqArray` return ()

	-- Use the boundary condition values as the initial matrix.
	let matInitial	= matBoundValue

	-- Run the solver.
	let matFinal	= solveLaplace steps matBoundMask matBoundValue matInitial

	matFinal
	  `deepSeqArray` return ()

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
	
	
-- | Solver for the Laplace equation.
solveLaplace
	:: Int			-- ^ Number of iterations to use.
	-> Array DIM2 Double	-- ^ Boundary value mask.
	-> Array DIM2 Double	-- ^ Boundary values.
	-> Array DIM2 Double	-- ^ Initial state.
	-> Array DIM2 Double

solveLaplace steps !arrBoundMask !arrBoundValue arrInit
 = go steps arrInit
 where	go s !arr
          | s == 0    = arr
          | otherwise = go (s-1)
		$! force
			(applyBoundary arrBoundMask arrBoundValue
			$  relaxLaplace  arr)


-- | Perform matrix relaxation for the Laplace equation,
--	using a stencil function.
--
--   Computation fn is
--	u'(i,j) = (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1)) / 4
--
relaxLaplace
	:: Array DIM2 Double
	-> Array DIM2 Double

{-# INLINE relaxLaplace #-}
relaxLaplace !arr
 = traverse arr id elemFn
 where
	_ :. height :. width	
		= extent arr

	{-# INLINE elemFn #-}
	elemFn get d@(sh :. i :. j)
	 = if isBorder i j
		 then  get d
		 else (get (sh :. (i-1) :. j)
		   +   get (sh :. i     :. (j-1))
		   +   get (sh :. (i+1) :. j)
	 	   +   get (sh :. i     :. (j+1))) / 4

	-- Check if this element is on the border of the matrix.
	-- If so we can't apply the stencil because we don't have all the neighbours.
	{-# INLINE isBorder #-}
	isBorder i j
	 	=  (i == 0) || (i >= width  - 1) 
	 	|| (j == 0) || (j >= height - 1) 


-- | Apply the boundary conditions to this matrix.
--	The mask  matrix has 0 in places where boundary conditions hold
--	and 1 otherwise.
--
--	The value matrix has the boundary condition value in places where it holds,
--	and 0 otherwise.
-- 
applyBoundary
	:: Array DIM2 Double	-- ^ Boundary condition mask.
	-> Array DIM2 Double	-- ^ Boundary condition values.
	-> Array DIM2 Double	-- ^ Initial matrix.
	-> Array DIM2 Double	-- ^ Matrix with boundary conditions applied.

{-# INLINE applyBoundary #-}
applyBoundary arrBoundMask arrBoundValue arr
 	= A.zipWith (+) arrBoundValue
	$ A.zipWith (*) arrBoundMask  arr


