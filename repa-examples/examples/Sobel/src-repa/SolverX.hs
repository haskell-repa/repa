{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module SolverX
	( gradientX_only)
where
import Data.Array.Repa 			as Repa
import Data.Array.Repa.Stencil
import Solver

gradientX_only :: Image -> Image
{-# NOINLINE gradientX_only #-}
gradientX_only img@Manifest{}
 	= img `deepSeqArray` forceBlockwise 
 	$ forStencil2 BoundClamp img
	  [stencil2|	-1  0  1
			-2  0  2
			-1  0  1 |]
