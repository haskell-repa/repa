{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module SolverX
	( gradientX_only)
where
import Data.Array.Repa
import Data.Array.Repa.Stencil
import Solver

gradientX_only :: Image -> Image
{-# NOINLINE gradientX_only #-}
gradientX_only img@(Array _ [Region RangeAll (GenManifest vec)])
 	= img `deepSeqArray` vec `seq` force2
 	$ forStencil2 BoundClamp img
	  [stencil2|	-1  0  1
			-2  0  2
			-1  0  1 |]
