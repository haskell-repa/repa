{-# LANGUAGE ParallelListComp, PatternGuards #-}

module Main where

import FFT
import Roots
import StrictComplex

import Data.Array.Repa	as A
import Data.List	as L
import Prelude		as P
import Control.Monad
import System.Environment


-- Arg Parsing ------------------------------------------------------------------------------------
data Arg
	= ArgVector	Int
	| ArgRandom
	| ArgStep	Int
	deriving Show


parseArgs []		= []
parseArgs (flag:xx)
	| "-vector"	<- flag
	, len:rest	<- xx
	= ArgVector (read len) : parseArgs rest
	
	| "-random"	<- flag
	, rest		<- xx
	= ArgRandom : parseArgs rest
	
	| "-step"	<- flag
	, onlen:rest		<- xx
	= ArgStep (read onlen) : parseArgs rest
	
	| otherwise	
	= error $ "bad arg " ++ flag ++ "\n"

help	= unlines
	[ "Usage: fft [args..]"
	, ""
	, "  -vector <length>           Transform a 1D vector."
	, ""
	, "  -random                    Use random data for the input."
	, "  -step   <on-length>        Use a step function for the input."
	, ""]


-- Main -------------------------------------------------------------------------------------------
main :: IO ()
main 
 = do	args	<- liftM parseArgs $ getArgs
	main' args

main' args

	-- A real-valued step function 
	| [vecLength]	<- [vecLength | ArgVector vecLength 	<- args]
	, [onLength]	<- [onLength  | ArgStep   onLength	<- args]
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
		fftMags	 = P.map mag $ A.toList $ fft roots arrInput

		str	= P.concat
			$ [show i ++ " " ++ show s ++ " " ++ show f ++ "\n"
				| i <- [0..]
				| s <- step_real
				| f <- fftMags ]
		
	  in	putStr str

	-- Not sure what you mean...
	| otherwise
	= putStr help
	
	