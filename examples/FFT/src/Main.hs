{-# LANGUAGE ParallelListComp, PatternGuards, ScopedTypeVariables #-}

module Main where

import FFT
import DFT
import Roots
import StrictComplex
import Vector
import Matrix
import PPM
import ColorRamp

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

	-- | Read a PPM file as input.
	| ArgPPM 
		FilePath

	-- | Write the result to this file.
	| ArgOutMagnitude
		FilePath

	-- | Write the magnitude of the transformed matrix as a PPM file.
	| ArgOutPPMMagnitude
		FilePath
		
	-- | Clip tranformed values to this level.
	| ArgOutPPMClip
		Double
		
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
		
	| "-ppm"		<- flag
	, fileName:rest		<- xx
	= ArgPPM fileName : parseArgs rest

	| "-ppm-clip"		<- flag
	, num:rest		<- xx
	= ArgOutPPMClip (read num) : parseArgs rest
	
	| "-out-magnitude"	<- flag
	, fileName:rest		<- xx
	= ArgOutMagnitude fileName : parseArgs rest
	
	| "-out-ppm-magnitude"	<- flag
	, fileName:rest		<- xx
	= ArgOutPPMMagnitude fileName : parseArgs rest
	
	| "-out-ppm-clip"	<- flag
	, level:rest		<- xx
	= ArgOutPPMClip (read level) : parseArgs rest
	
	| otherwise	
	= error $ "bad arg " ++ flag ++ "\n"


help	= unlines
	[ "Usage: fft [args..]"
	, ""
	, "  -dft                                      Use (slow) DFT instead of (fast) FFT."
	, "  -inverse                                  Compute the inverse transform."
	, ""
	, "INPUT:"
	, "  -vector-real      <filename>              Read a real valued input vector from this file."
	, "  -vector-real-step <onlen::Int> <len::Int> Use a real valued step function for input."
	, "  -ppm              <filename>              Use a PPM file for input."
	, ""
	, "OUTPUT:"
	, "  -out-vector-magnitude <file-name>         Write the magnitute of the transformed vector to file."
	, "  -out-ppm-magnitude    <file-name>         Write transformed matrix to a ppm file."
	, "  -out-ppm-clip         <val::Int>           ... while clipping transformed values to this level."                  
	, ""
	, "NOTE:" 
	, "  - For the fast algorithm, the length of the input vector/dimensions of the array"
	, "    must be powers of two."
	, ""
	, "  - When using PPM input files, pixels are converted to grey-scale and then used as"
	, "    the real values of the input matrix." ]
	
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
	| [fileName]	<- [f	| ArgVectorReal f <- args]
	= do	arr_real :: Array DIM1 Double	<- readVectorFromTextFile fileName 
		let arr		= A.map (\r -> r :*: 0) arr_real
		let arrT	= alg arr
		outVector args arrT
	
	-- | Transform a PPM file.
	| [fileName]	<- [f	| ArgPPM f <- args]
	= do	let loadPixel r g b = sqrt (fromIntegral r^2 + fromIntegral g^2 + fromIntegral b^2)
		arr_double	<- readPPMAsMatrix loadPixel fileName
		let arr_real	= (A.map (\r -> r :*: 0) arr_double) :: Array DIM2 Complex
		let arrT 	= fft2d arr_real

		outPPM args arrT
	
	-- Not sure what you mean...
	| otherwise
	= putStr help
	
	
outVector args vec
	| [fileName]	<- [f | ArgOutMagnitude f <- args ]
	= writeVectorAsTextFile (A.map mag vec) fileName
			
	| otherwise
	= return ()

outPPM :: [Arg] -> Array DIM2 Complex -> IO ()
outPPM args arr
	| [fileName]	<- [f | ArgOutPPMMagnitude f <- args ]
	, mClipLevel	<- listToMaybe [l | ArgOutPPMClip l <- args]
	= do	let arr_mag	= A.map mag arr
		let arr_clipped	= maybe arr_mag
					(\level -> A.map (\x -> if x > level then level else x) arr_mag)
					mClipLevel
					
		writeMatrixAsNormalisedPPM fileName 
		 	pixelGrey arr_clipped
		
		
pixelColor x = (x, x, x)
pixelGrey  x = (x, x, x)

