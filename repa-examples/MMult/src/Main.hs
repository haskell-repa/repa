{-# LANGUAGE PatternGuards #-}

import Data.Array.Repa			as A
import Data.Array.Repa.IO.Matrix
import Data.Array.Repa.Algorithms.Matrix
import Data.Maybe
import System.Environment
import Control.Monad
import System.Random
import qualified Data.Array.Parallel.Unlifted as U

-- Arg Parsing ------------------------------------------------------------------------------------
data Arg
	= ArgSolver       String
	| ArgMatrixRandom Int Int
	| ArgMatrixFile   FilePath
	| ArgOutFile	  FilePath
	deriving Show

isArgMatrix arg
 = case arg of
	ArgMatrixRandom{}	-> True
	ArgMatrixFile{}		-> True
	_			-> False

parseArgs []		= []
parseArgs (flag:xx)
	| "-file"	<- flag
	, f:rest	<- xx
	= ArgMatrixFile f : parseArgs rest

	| "-out"	<- flag
	, f:rest	<- xx
	= ArgOutFile f	: parseArgs rest
	
	| "-random"	<- flag
	, x:y:rest	<- xx
	= ArgMatrixRandom (read x) (read y) : parseArgs rest
	
	| otherwise	
	= error $ "bad arg " ++ flag ++ "\n"

printHelp
	= putStr 	
	$ unlines
	[ "Usage: mmult [args..]"
	, ""
	, "  -random <height> <width>   Use a random matrix of this size."
	, "  -file   <filename>         Read a matrix from this file."
	, "  -out    <filename>         Write resulting matrix to this file."
	, ""
	, "  Format of matrix file:"
	, "    MATRIX"
	, "    <width> <height>"
	, "    <whitespace separated values..>"
	, "" ]


-- | Get a matrix from a file, or generate a random one.
getMatrix :: Arg -> IO (Array DIM2 Double)
getMatrix arg
 = case arg of
	ArgMatrixFile   fileName	
	 -> readMatrixFromTextFile fileName

	ArgMatrixRandom height width	
	 -> genRandomMatrix (Z :. height :. width)	


-- Random -----------------------------------------------------------------------------------------
-- | Generate a random(ish) matrix.
genRandomMatrix 
	:: DIM2 
	-> IO (Array DIM2 Double)

genRandomMatrix sh
 = do	uarr	<- genRandomUArray (A.size sh)
	return	$ fromUArray sh uarr

-- | Generate a random(ish) UArray of doubles.
-- The std random function is too slow to generate really big vectors
-- with.  Instead, we generate a short random vector and repeat that.
genRandomUArray :: Int -> IO (U.Array Double)
genRandomUArray n 
 = do	let k		= 1000
    	rg		<- newStdGen
    	let randvec	= U.randomRs k (-100, 100) rg
	let vec		= U.map (\i -> randvec U.!: (i `mod` k)) (U.enumFromTo 0 (n-1))
	return vec

			
-- Main -------------------------------------------------------------------------------------------
main :: IO ()
main 
 = do	args	<- liftM parseArgs $ getArgs
	main' args

main' args
	| [argMat1, argMat2]	<- filter isArgMatrix args
	, mArgOut		<- listToMaybe [s | ArgOutFile s <- args]
	= do	
		-- Get matrices from files, 
		-- or generate random ones we were asked to.
		mat1		<- getMatrix argMat1
		mat2		<- getMatrix argMat2

        	mat1
          	 `deepSeqArray` mat2
          	 `deepSeqArray` return ()
		
		-- Run the solver.
		let matResult	= multiplyMM mat1 mat2

		matResult `deepSeqArray` return ()

		-- Write the output to file if requested.
		case mArgOut of 
		 Nothing	-> return ()
		 Just fileOut	-> writeMatrixToTextFile fileOut matResult
					
	| otherwise
	= printHelp


