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
	-- General Setup ------------------------
	-- | Use DFT instead of FFT (very slow)
	= ArgDFT			

	-- | Compute the inverse transform initially.
	| ArgInverse

	-- Input --------------------------------
	-- | Use a vector step function for input.
	| ArgInVectorStep	
		Int		-- length of 'on' part
		Int		-- total length of vector

	-- | Read the a real valued vector from this file.
	| ArgInVectorReal
		FilePath

	-- | Read a file as an input matrix.
	| ArgInMatrixReal
		FilePath

	-- Transforms ---------------------------
	-- | Clip transformed values to this level.
	| ArgTransClip
		Double

	-- Output ------------------------------
	-- | Write the magnitue of the transformed values.
	| ArgOutTransMag
		FilePath

	-- | Clip the magnitude of transformed values to this level
	| ArgOutTransMagClip
		Double

	-- | Put the zero value in the center of the transformed image / vector.
	| ArgOutTransCentered
		
	-- | Perform the inverse transform
	--	and write the the magnitude of the result to this file.
	| ArgOutInverseMag
		FilePath
	
	deriving (Show, Eq)


parseArgs []		= []
parseArgs (flag:xx)

	-- General Setup ------------------------
	| "-dft"		<- flag
	= ArgDFT : parseArgs xx

	| "-inverse"		<- flag
	= ArgInverse : parseArgs xx


	-- Input --------------------------------
	| "-in-vector-real-step"<- flag
	, onlen:len:rest	<- xx
	= ArgInVectorStep (read onlen) (read len) : parseArgs rest

	| "-in-vector-real"   	<- flag
	, fileName:rest		<- xx
	= ArgInVectorReal fileName : parseArgs rest

	| "-in-matrix-real"	<- flag
	, fileName:rest		<- xx
	= ArgInMatrixReal fileName : parseArgs rest
		
	-- Transforms ---------------------------
	| "-trans-clip"	<- flag
	, level:rest		<- xx
	= ArgTransClip (read level) : parseArgs rest
	
	-- Output -------------------------------
	| "-out-trans-mag"	<- flag
	, fileName:rest		<- xx
	= ArgOutTransMag fileName : parseArgs rest

	| "-out-trans-mag-clip"	<- flag
	, level:rest		<- xx
	= ArgOutTransMagClip (read level) : parseArgs rest

	| "-out-trans-centered"		<- flag
	= ArgOutTransCentered : parseArgs xx
	
	| "-out-inverse-mag"	<- flag
	, fileName:rest		<- xx
	= ArgOutInverseMag fileName : parseArgs rest
	
	-- Sorry
	| otherwise	
	= error $ "unknown argument " ++ flag ++ "\n"


help	= unlines
	[ "Usage: fft [args..]"
	, "  -inverse                                 Compute the inverse transform initially."
	, ""
	, "INPUT:"
	, "  -in-vector-real  <filename>              Read a real valued input vector from this file."
	, "  -in-vector-step  <onlen::Int> <len::Int> Use a real valued step function for input."
	, "  -in-matrix-real  <filename>              Read a real valued input matrix from this file."
	, ""
	, "FREQUENCY SPACE TRANSFORMS:"
	, "  -trans-clip      <level::Double>         Clip both re/im values to this level"
	, ""
	, "OUTPUT:"
	, "  -out-trans-mag      <filename>           Write the magnitute of the transformed values to file."
        , "  -out-trans-mag-clip <level::Double>      Clip the output values to this level."
	, "  -out-trans-ceneted                       Place the zero frequency in the center of the output."
	, "  -out-inverse-mag    <filename>           Also perform the inverse transform and write to file."
	, ""
	, "NOTE:" 
	, "  - For the fast algorithm, the dimensions of the input must be powers of two."
	, "  - When using BMP input files, pixels are converted to grey-scale and then used as"
	, "    the real values of the input matrix." 
	, "" ]
	
-- Main -------------------------------------------------------------------------------------------
main :: IO ()
main 
 = do	args	<- liftM parseArgs $ getArgs
	mainInput args


mainInput args
	-- Generate a real valued step vector.
	| [(onLength, vecLength)]	
		<- [(ol, vl) | ArgInVectorStep ol vl <- args]
	= let	
		offLength	= vecLength - onLength
		list_real	= P.replicate onLength 1 ++ P.replicate offLength 0
		list_complex	= P.map (:*: 0) list_real
		arr		= A.fromList (Z :. vecLength) list_complex
	  in	mainTransVector args arr
		
	-- Read a matrix from a file.
	| [fileName]	
		<- [f	| ArgInVectorReal f <- args]
	= do	arr_real :: Array DIM1 Double	
			<- readVectorFromTextFile fileName 
		let arr	= A.map (\r -> r :*: 0) arr_real
		mainTransVector args arr
	
	-- Read a matrix from a file.
	| [fileName]	
		<- [f	| ArgInMatrixReal f <- args]
	= do	arr_real	<- readMatrixFromBMP fileName
		let arr		= force $ (A.map (\r -> r :*: 0) arr_real) :: Array DIM2 Complex		
		mainTransMatrix args arr

	-- Dunno...
	| otherwise
	= putStr help

-- Trans ------------------------------------------------------------------------------------------	
-- Apply the centering transform if needed.

mainTransVector args arr
	| elem ArgOutTransCentered args
	= mainFFTVector args (error "finish me") -- centerifyMatrix arr)
	
	| otherwise
	= mainFFTVector args arr

mainTransMatrix :: [Arg] -> Array DIM2 Complex -> IO ()	
mainTransMatrix args arr
	| elem ArgOutTransCentered args
	= mainFFTMatrix args (centerifyMatrix arr)
	
	| otherwise
	= mainFFTMatrix args arr


-- | Apply a transform to the input matrix that causes the output
--	image to be centered on the zero value.
centerifyMatrix
	:: Array DIM2 Complex
	-> Array DIM2 Complex

{-# INLINE centerifyMatrix #-}
centerifyMatrix arr
 = traverse arr id
	(\get ix@(_ :. y :. x) -> ((-1) ^ (y + x)) * get ix)
	

-- FFT --------------------------------------------------------------------------------------------
-- Transform to frequency space.

mainFFTVector args arr
 = let	alg	= if elem ArgInverse args
			then fft
			else ifft
				
	arrT	= alg arr
   in	mainMungeVector args arrT

mainFFTMatrix :: [Arg] -> Array DIM2 Complex -> IO ()
mainFFTMatrix args arr
 = let	alg	= if elem ArgInverse args
			then ifft2d
			else fft2d
	
	arrT	= force (alg arr)
   in 	mainMungeMatrix args arrT


-- Munge ------------------------------------------------------------------------------------------
-- Munge the values in frequency space.

mainMungeVector args arr
	= mainOutVector args arr
	

mainMungeMatrix :: [Arg] -> Array DIM2 Complex -> IO ()	
mainMungeMatrix args arr
 = arr `deepSeqArray` 
   let	mungeClip :: Array DIM2 Complex -> Array DIM2 Complex
	mungeClip 
		= maybe id
			(\level -> A.map (clipComplex level))
			(listToMaybe [l | ArgTransClip l <- args])

   in	mainOutMatrix args (mungeClip arr)

	
{-# INLINE clipComplex #-}
clipComplex level (r :*: i)
 = let	r'	= if r > level then level else r
	i'	= if i > level then level else i
   in	(r' :*: i')
	

-- Output -----------------------------------------------------------------------------------------	
-- Write transformed values to a file.

mainOutVector args arr
	| [fileName]	<- [f | ArgOutTransMag f <- args ]
	= writeVectorAsTextFile (A.map mag arr) fileName
			
	| otherwise
	= return ()

mainOutMatrix args arr
	| [fileName]	<- [f | ArgOutTransMag f <- args ]
	= let	clip	= maybe id
				(\level -> (\x -> if x > level then level else x))
				(listToMaybe [l | ArgOutTransMagClip l <- args])
		
		arr'	= force $ A.map (clip . mag) arr
	  in do	
		arr' `deepSeqArray`
		 writeMatrixToGreyscaleBMP fileName arr'

		mainOutInverseMatrix args arr

	| otherwise
	= mainOutInverseMatrix args arr

-- Inverse ----------------------------------------------------------------------------------------
mainOutInverseMatrix args arr
	| [fileName]	<- [f | ArgOutInverseMag f <- args]
	= let	alg	= if elem ArgInverse args
				then fft2d
				else ifft2d
				
		arrT	= force $ alg arr
		arrMag	= arrT `deepSeqArray` A.map mag arrT
	  in	writeMatrixToGreyscaleBMP fileName arrMag

	| otherwise
	= return ()