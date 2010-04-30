{-# LANGUAGE ParallelListComp, PatternGuards, ScopedTypeVariables #-}

module Main where

import FFT
import DFT
import Roots
import StrictComplex
import Vector
import Matrix
import PPM
import BMP
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
	
	-- | Put the zero frequency in the center of the transformed vector / matrix.
	| ArgCentered
	
	-- | Use a vector step function for input.
	| ArgVectorRealStep	
		Int		-- length of 'on' part
		Int		-- total length of vector

	-- | Read the a real valued vector from this file.
	| ArgVectorReal
		FilePath

	-- | Read a PPM file as input.
	| ArgBMP
		FilePath

	-- | Write the result to this file.
	| ArgOutMagnitude
		FilePath

	-- | Write the magnitude of the transformed matrix as a PPM file.
	| ArgOutBMPMagnitude
		FilePath
		
	-- | Clip tranformed values to this level.
	| ArgOutBMPClip
		Double
		
	deriving (Show, Eq)


parseArgs []		= []
parseArgs (flag:xx)
	| "-dft"		<- flag
	= ArgDFT : parseArgs xx

	| "-inverse"		<- flag
	= ArgInverse : parseArgs xx

	| "-centered"		<- flag
	= ArgCentered : parseArgs xx

	| "-vector-real-step"	<- flag
	, onlen:len:rest	<- xx
	= ArgVectorRealStep (read onlen) (read len) : parseArgs rest

	| "-vector-real"   	<- flag
	, fileName:rest		<- xx
	= ArgVectorReal fileName : parseArgs rest
		
	| "-bmp"		<- flag
	, fileName:rest		<- xx
	= ArgBMP fileName : parseArgs rest
	
	| "-out-magnitude"	<- flag
	, fileName:rest		<- xx
	= ArgOutMagnitude fileName : parseArgs rest
	
	| "-out-bmp-magnitude"	<- flag
	, fileName:rest		<- xx
	= ArgOutBMPMagnitude fileName : parseArgs rest
	
	| "-out-bmp-clip"	<- flag
	, level:rest		<- xx
	= ArgOutBMPClip (read level) : parseArgs rest
	
	| otherwise	
	= error $ "bad arg " ++ flag ++ "\n"


help	= unlines
	[ "Usage: fft [args..]"
	, ""
	, "  -dft               Use (slow) DFT instead of (fast) FFT."
	, "  -inverse           Compute the inverse transform."
	, "  -centered          Put the zero frequency in the center of the transformed vector/matrix."
	, ""
	, "INPUT:"
	, "  -vector-real      <filename>              Read a real valued input vector from this file."
	, "  -vector-real-step <onlen::Int> <len::Int> Use a real valued step function for input."
	, "  -bmp              <filename>              Use a BMP file for input."
	, ""
	, "OUTPUT:"
	, "  -out-vector-magnitude <file-name>         Write the magnitute of the transformed vector to file."
	, "  -out-bmp-magnitude    <file-name>         Write transformed matrix to a bmp file."
	, "  -out-bmp-clip         <val::Int>           ... while clipping transformed values to this level."                  
	, ""
	, "NOTE:" 
	, "  - For the fast algorithm, the length of the input vector/dimensions of the array"
	, "    must be powers of two."
	, ""
	, "  - When using BMP input files, pixels are converted to grey-scale and then used as"
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
	
	-- | Transform a BMP file.
	| [fileName]	<- [f	| ArgBMP f <- args]
	= do	arr_double	<- readMatrixFromBMP fileName
		let arr_real	= (A.map (\r -> r :*: 0) arr_double) :: Array DIM2 Complex
		let arr_centered
			= if elem ArgCentered args 
				then centerifyMatrix arr_real
				else arr_real
					
		let arrT = fft2d arr_centered

		outBMP args arrT
	
	-- Not sure what you mean...
	| otherwise
	= putStr help
	
	
outVector args vec
	| [fileName]	<- [f | ArgOutMagnitude f <- args ]
	= writeVectorAsTextFile (A.map mag vec) fileName
			
	| otherwise
	= return ()


centerifyMatrix
	:: Array DIM2 Complex
	-> Array DIM2 Complex
centerifyMatrix arr
 = traverse arr id
	(\get ix@(_ :. y :. x) -> ((-1) ^ (y + x)) * get ix)


outBMP :: [Arg] -> Array DIM2 Complex -> IO ()
outBMP args arr
	| [fileName]	<- [f | ArgOutBMPMagnitude f <- args ]
	, mClipLevel	<- listToMaybe [l | ArgOutBMPClip l <- args]
	= do	let arr_mag	= A.map mag arr
		let arr_clipped	
			= maybe arr_mag
				(\level -> A.map (\x -> if x > level then level else x) arr_mag)
				mClipLevel
					
		writeMatrixToGreyscaleBMP 
			fileName arr_clipped


