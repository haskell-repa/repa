{-# LANGUAGE ParallelListComp, PatternGuards #-}

module Main where

import FFT
import DFT
import Roots
import StrictComplex

import Data.Array.Repa	as A
import Data.List	as L
import Prelude		as P
import Control.Monad
import System.Environment


-- Arg Parsing ------------------------------------------------------------------------------------
data Arg
	-- | Use DFT instead of FFT
	= ArgDFT			
	
	-- | Use a vector step function for input.
	| ArgVectorStep	
		Int		-- length of 'on' part
		Int		-- total length of vector

	-- | Use a random vector for input
	| ArgVectorRandom
		Int		-- length of vector.

	deriving (Show, Eq)


parseArgs []		= []
parseArgs (flag:xx)
	| "-dft"		<- flag
	= ArgDFT : parseArgs xx

	| "-vector-step"	<- flag
	, onlen:len:rest	<- xx
	= ArgVectorStep (read onlen) (read len) : parseArgs rest
	
	| "-vector-random"	<- flag
	, len:rest		<- xx
	= ArgVectorRandom (read len) : parseArgs rest
		
	| otherwise	
	= error $ "bad arg " ++ flag ++ "\n"

help	= unlines
	[ "Usage: fft [args..]"
	, ""
	, "  -dft                                  Use (slow) DFT instead of (fast) FFT."
	, "  -vector-step   <on-length> <length>   Use a step function for input."
	, "  -vector-random <length>               Use a random vector for input."
	, ""]


-- Main -------------------------------------------------------------------------------------------
main :: IO ()
main 
 = do	args	<- liftM parseArgs $ getArgs
	
	-- Decide which algorithm to use
	let alg
		| elem ArgDFT args	= dft
		| otherwise		= fft

	main' args alg


main' args alg

	-- Transform a real-valued step function 
	| [(onLength, vecLength)]	 <- [(ol, vl) | ArgVectorStep ol vl <- args]
	= let
		offLength	= vecLength - onLength
		step_real	= P.replicate onLength 1 ++ P.replicate offLength 0
		step		= P.map (:*: 0) step_real

		-- Calculate roots to use for this lengthed vector
		shape :: DIM1 
		shape	= Z :. length step	

		roots :: Array DIM1 Complex
		roots	= calcRofu shape

		-- Convert input to an Array so we can feed it to the algs.
		arrInput = A.fromList shape step

		-- Compute DFT and FFT and to compare.
		fftMags	 = P.map mag $ A.toList $ alg roots arrInput

		str	= P.concat
			$ [show i ++ " " ++ show s ++ " " ++ show f ++ "\n"
				| i <- [0..]
				| s <- step_real
				| f <- fftMags ]
		
	  in	putStr str

	-- Not sure what you mean...
	| otherwise
	= putStr help
	
	