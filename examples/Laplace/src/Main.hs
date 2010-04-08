{-# LANGUAGE BangPatterns #-}

-- | Solver for the Laplace equation
--	Writes a PPM file of the completed solution.
--
--	You can use the ImageMagick convert program to make a png
--	with	"convert out.ppm out.png"
--
import qualified Data.Array.Parallel.Unlifted 	as U
import Prelude					as P
import Data.List				as L
import Data.Maybe
import System.Environment

import Bench.Benchmark ( time, showTime )

import PPM
import ColorRamp

import Array					as A
import SolveArray				as A

import DArray					as DA
import SolveDArray				as DA

import qualified CArray				as CA
import qualified SolveCArray			as CA

import qualified CArrayFlatDim			as CAF
import qualified SolveCArrayFlatDim		as CAF

import qualified SolveMArray			as MA


-- Solvers ----------------------------------------------------------------------------------------
type Solver
	= Int 				-- ^ Number of steps to use.
	-> Array DIM2 Double		-- ^ Boundary condition mask
	-> Array DIM2 Double		-- ^ Boundary condition value.
	-> Array DIM2 Double 		-- ^ Initial matrix.
	-> Array DIM2 Double

algorithms
  =	[ ("array-shift-poly",		  A.solve  A.relaxLaplace_shift)
	, ("array-backpermute-poly",	  A.solve  A.relaxLaplace_backpermute)
	, ("darray-shift-poly",		 DA.solve DA.relaxLaplace_shift)
	, ("darray-stencil-poly",	 DA.solve DA.relaxLaplace_stencil)
	, ("darray-stencil",		 DA.solveLaplace_stencil)
	, ("carray-stencil",		 CA.solveLaplace_stencil)
	, ("carray-stencil-flatdim",	CAF.solveLaplace_stencil)
	, ("marray-stencil",		 MA.solveLaplace_stencil)
	]


-- Main -------------------------------------------------------------------------------------------
main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [solverName, steps, fileInput, fileOutput]	
	   -> do
		let badSolver
			= error 
			$  "unknown solver: " ++ solverName ++ "\n"
			++ "choose one of: " ++ show (L.map fst algorithms) ++ "\n"

		let solver	
			= fromMaybe badSolver						
			$ lookup solverName algorithms
			
		laplace solver (read steps) 
			fileInput
			fileOutput

	  _ -> do
		putStr 	$ unlines
			[ "Usage: laplace <solver> <iterations> <input.ppm> <output.ppm>"
			, ""
			, "  solvers:\n" 
				++ concat ["     " ++ name ++ "\n" | (name, _) <- algorithms]
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
			
			
		return ()


laplace :: Solver
	-> Int			-- ^ Number of iterations to use.
	-> FilePath 		-- ^ Input file.
	-> FilePath		-- ^ Output file
	-> IO ()

laplace solver steps fileInput fileOutput
 = do
	(matBoundMask, matBoundValue)	
			<- readPPMAsMatrix2 loadPixel fileInput

	let matInitial	= matBoundValue

        matBoundMask
          `deepSeqArray` matBoundValue
          `deepSeqArray` return ()

        (matFinal, t) <- time
                       $ let matFinal	= solver
				steps
				matBoundMask 
				matBoundValue 
				matInitial
                         in
                         matFinal `deepSeqArray` return matFinal

	-- Write out the matrix as a colorised PPM image	
	writeMatrixAsNormalisedPPM
		fileOutput
		(rampColorHotToCold 0.0 1.0)
		matFinal

        putStrLn (showTime t)


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
	
	
-- Initial Value ----------------------------------------------------------------------------------
-- | Make the initial value for the matrix.
mkInitialValue :: Int -> DIM2 -> Double
mkInitialValue _ _
	= 0


-- Boundary Conditions ----------------------------------------------------------------------------
-- | Make the mask for the boundary conditions.
--	Should return 0 when the point is part of the boundary, and 1 otherwise.
mkBoundaryMask :: Int -> DIM2 -> Double
mkBoundaryMask size (() :. x :. y)	
	| x == 0		= 0
	| y == 0		= 0
	| x >= (size - 1)	= 0
	| y >= (size - 1)	= 0
	| otherwise		= 1


-- | Make the values for the boundary conditions.
--	Should return 0 where the point is not part of the boundary.
mkBoundaryValue :: Int -> DIM2 -> Double
mkBoundaryValue size (() :. x :. y)
	| x == 0 && y > 0 && y < n 	= 80
	| y == 0 && x > 0 && x < n	= 20
	| x == n && y > 0 && y < n 	= 0
	| y == n && x > 0 && x < n	= 180
	| otherwise			= 0
	where	n	= size - 1



---------------------------------------------------------------------------------------------------
-- | Given a function that produces each element, 
--	create a matrix of a given size.
createMatrix 
	:: DIM2 			-- ^ Size of matrix.
	-> (DIM2 -> Double) 		-- ^ fn to produce each element.
	-> Array DIM2 Double

createMatrix dim mkElem
 = let	() :. width :. height	= dim
	arrList	= [mkElem (() :. x :. y)
			| y <- [0 .. (width - 1)]
			, x <- [0 .. (height - 1)]]
   in	toArray dim $ U.fromList arrList




