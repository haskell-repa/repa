{-# LANGUAGE ParallelListComp, PatternGuards, ScopedTypeVariables #-}

module Main where

import FFT
import DFT
import Roots
import StrictComplex
import Vector

import Data.Array.Repa	as A
import Data.List	as L
import Data.Maybe	
import Prelude		as P
import Control.Monad
import System.Environment


-- Arg Parsing ------------------------------------------------------------------------------------
data Arg
	-- | Use DFT instead of FFT
	= ArgDFT			

	-- | Compute the inverse transform.
	| ArgInverse
	
	-- | Use a vector step function for input.
	| ArgVectorRealStep	
		Int		-- length of 'on' part
		Int		-- total length of vector

	-- | Read the a real valued vector from this file.
	| ArgVectorReal
		FilePath

	-- | Write the result to this file.
	| ArgOutMagnitude
		FilePath

	deriving (Show, Eq)


parseArgs []		= []
parseArgs (flag:xx)
	| "-dft"		<- flag
	= ArgDFT : parseArgs xx

	| "-inverse"		<- flag
	= ArgInverse : parseArgs xx

	| "-vector-real-step"	<- flag
	, onlen:len:rest	<- xx
	= ArgVectorRealStep (read onlen) (read len) : parseArgs rest

	| "-vector-real"   	<- flag
	, fileName:rest		<- xx
	= ArgVectorReal fileName : parseArgs rest
		
	| "-out-magnitude"	<- flag
	, fileName:rest		<- xx
	= ArgOutMagnitude fileName : parseArgs rest
		
	| otherwise	
	= error $ "bad arg " ++ flag ++ "\n"


help	= unlines
	[ "Usage: fft [args..]"
	, ""
	, "  -dft                                     Use (slow) DFT instead of (fast) FFT."
	, "  -inverse                                 Compute the inverse transform."
	, "  -vector-real      <file-name>            Read a real valued input vector from this file."
	, "  -vector-real-step <on-length> <length>   Use a real valued step function for input."
	, "  -out-magnitude    <file-name>            Write the magnitute of the output to this file."
	, ""
	, "  NOTE: For the fast algorithm, the length of the input vector/dimensions of the array"
	, "        must be powers of two." ]
	


-- Main -------------------------------------------------------------------------------------------
main :: IO ()
main 
 = do	args	<- liftM parseArgs $ getArgs
	
	-- Decide which algorithm to use
	let alg	| elem ArgDFT args	
		, elem ArgInverse args	= idft

		| elem ArgDFT args	= dft
		
		| elem ArgInverse args	= ifft
			
		| otherwise		= fft

	main' args alg


main' args alg

	-- | Transform a real-valued step function 
	| [(onLength, vecLength)]	<- [(ol, vl) | ArgVectorRealStep ol vl <- args]
	= let	offLength	= vecLength - onLength
		step_real	= P.replicate onLength 1 ++ P.replicate offLength 0
		step		= P.map (:*: 0) step_real
		
		arr	= A.fromList (Z :. vecLength) step
		arrT	= alg arr
	  in	outVector args arrT
	
	-- | Transform some vector from a file
	| [fileName]				<- [f	| ArgVectorReal f <- args]
	= do	arr_real :: Array DIM1 Double	<- readVectorFromTextFile fileName 
		let arr		= A.map (\r -> r :*: 0) arr_real
		let arrT	= alg arr
		outVector args arrT
	
	-- Not sure what you mean...
	| otherwise
	= putStr help
	
	
outVector args vec
	| Just fileName	<- listToMaybe [s | ArgOutMagnitude s <- args ]
	= writeVectorAsTextFile (A.map mag vec) fileName
			
	| otherwise
	= return ()
	
	
	
	
	
	
	
	
	
	
	
	