{-# LANGUAGE ParallelListComp #-}

module Main
where

import FFTCArray
import StrictComplex
import Data.Array.Parallel.Base ( (:*:)(..) )
import Array			((:.)(..), DIM1, DIM3, Shape(..), Subshape(..))

import CArray			as CA
import Array 			as A

import Bench.Benchmark 		( timeFn_, showTime )
import Data.Char 		( toLower )
import System.Environment 	( getArgs )
import Prelude			as P
import Data.List		as L
import qualified Data.Array.Parallel.Unlifted as U


wrapFFT3d
	:: (Int -> CArray DIM3 Complex -> CArray DIM3 Complex)
	-> (Int, U.Array Double, U.Array Double) 	-- iterations, real part, imaginary paty
	-> U.Array Double

{-# NOINLINE wrapFFT3d #-}
wrapFFT3d f (n, xs, ys) 
	= U.snds
        $ carrayData
        $ f 1
        $ mkCArray (() :. n :. n :. n)
        $ U.zip xs ys

algs
 = 	[ ('d', fft3D)
	, ('s', fft3DS)
	, ('c', fft3DC)]


main 
 = do	args	<- getArgs
	case args of
	 ["check"]	-> putStrLn check
	 [[c],s] 
	  -> do	let n  = read s
             	    xs = U.map fromIntegral
                	$ U.enumFromTo 1 (n*n*n)

             	    fn = case lookup (toLower c) algs of
                    		Just f  -> f
                    		Nothing -> error $ "Unknown algorithm " ++ [c]

                xs `seq` fn `seq` return ()
 		t <- timeFn_ (wrapFFT3d fn) (`seq` ()) (n,xs,xs)
		putStrLn (showTime t)
		

check
 = let	
	-- A real-valued step function 
	step_real	= P.replicate 10 1 ++ P.replicate 118 0
	step		= P.map (:*: 0) step_real
	

	-- Calculate roots to use for this lengthed vector
	shape :: DIM1 
	shape	= () :. length step	

	roots :: CArray DIM1 Complex
	roots	= calcRofu shape

	-- Convert input to a CArray so we can feed it to the algs.
	arrInput = CA.toCArray $ A.toArray shape $ U.fromList step
	
	-- Compute DFT and FFT and to compare.
 	dftMags	 = P.map mag $ CA.toList $ dft roots arrInput
	fftMags	 = P.map mag $ CA.toList $ fft roots arrInput
	fftSMags = P.map mag $ CA.toList $ fftS roots arrInput
	
	str	= P.concat
		$ L.intersperse "\n"
		$ [show i ++ " " ++ show s ++ " " ++ show d ++ " " ++ show f ++ " " ++ show g
			| i <- [0..]
			| s <- step_real
			| d <- dftMags
			| f <- fftMags 
			| g <- fftSMags ]

  in	str

