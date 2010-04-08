{-# OPTIONS -fglasgow-exts #-}

import qualified Data.Array.Parallel.Unlifted 	as U
import Array					as A
import qualified SolveCArray			as CA
import qualified SolveDArray			as DA
import Data.List				as L
import Data.Maybe
import Bench.Benchmark ( time, showTime )
import Matrix
import System.Environment
import Control.Monad


-- Solvers ----------------------------------------------------------------------------------------
type Solver
	=  Array DIM2 Double		-- ^ First matrix.
	-> Array DIM2 Double		-- ^ Second matrix.
	-> Array DIM2 Double 		-- ^ Product matrix.

algorithms :: [(String, Solver)]
algorithms
  =	[ ("carray-replicate",		CA.wrapCArraySolver CA.mmMult_replicate) 
	, ("carray-traverse",		CA.wrapCArraySolver CA.mmMult_traverse) 
	, ("darray-replicate",		DA.wrapDArraySolver DA.mmMult_replicate)
	, ("darray-traverse",		DA.wrapDArraySolver DA.mmMult_traverse)]


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
	| "-solver"	<- flag
	, s:rest	<- xx
	= ArgSolver s	: parseArgs rest
	
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

			
-- Main -------------------------------------------------------------------------------------------
main :: IO ()
main 
 = do	args	<- liftM parseArgs $ getArgs

	-- | Load up the cmd line args.
	let result 
		| [solverName]		<- [s | ArgSolver s <- args]
		, solver		<- fromMaybe (badSolver solverName)
					$ lookup solverName algorithms
		, [argMat1, argMat2]	<- filter isArgMatrix args
		, mArgOut		<- listToMaybe [s | ArgOutFile s <- args]
		= do	

			-- Get matrices from files, or generate random ones we were asked to.
			mat1		<- getMatrix argMat1
			mat2		<- getMatrix argMat2

        		mat1
          		 `deepSeqArray` mat2
          		 `deepSeqArray` return ()
	
			-- Run the solver.
			(matResult, t)	<- time
					$  let matResult = solver mat1 mat2
			   		   in  matResult `deepSeqArray` return matResult

			-- Print how long it took.
			putStrLn (showTime t)

			-- Print a checksum of all the elements
			putStrLn $ "sum = " ++ show (A.toScalar $ A.sum $ A.sum matResult) ++ "\n"

			-- Write the output to file if requested.
			case mArgOut of 
			 Nothing	-> return ()
			 Just fileOut	
			  -> 	writeMatrixAsTextFile
					matResult fileOut
					

		| otherwise
		= printHelp

	result


getMatrix arg
 = case arg of
	ArgMatrixFile   fileName	
	 -> readMatrixFromTextFile fileName

	ArgMatrixRandom height width	
	 -> genRandomMatrix (() :. height :. width)
	

printHelp
	= putStr 	
	$ unlines
	[ "Usage: mmult [args..]"
	, ""
	, "  -solver <solver>           Set solver used for multiplication."
	, "  -random <height> <width>   Use a random matrix of this size."
	, "  -file   <filename>         Read a matrix from this file."
	, "  -out    <filename>         Write resulting matrix to this file."
	, ""
	, "  solvers:\n" 
	  ++ (concat $ intersperse "\n" ["     " ++ name | (name, _) <- algorithms])
	, "" 
	, "  You must specify at least the solver, and two input matrices."
	, ""
	, "  Format of matrix file:"
	, "    MATRIX"
	, "    <width> <height>"
	, "    <whitespace separated values..>"
	, "" ]
	
		
badSolver solverName
	= error 
	$ unlines
	[ "unknown solver: " ++ solverName
	, "choose one of: "  ++ show (L.map fst algorithms) ]


