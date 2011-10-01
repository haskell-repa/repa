{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module Solver 
	( Image
	, gradientX
	, gradientY )
where
import Data.Array.Repa 			as R
import Data.Array.Repa.Stencil          as R
import Data.Array.Repa.Stencil.Dim2     as R

type Image	= Array U DIM2 Float


gradientX :: Image -> Image
{-# NOINLINE gradientX #-}
gradientX img
 	= img `deepSeqArray` compute
 	$ forStencil2 (BoundConst 0) img
	  [stencil2|	-1  0  1
			-2  0  2
			-1  0  1 |]


gradientY :: Image -> Image
{-# NOINLINE gradientY #-}
gradientY img
	= img `deepSeqArray` compute
	$ forStencil2 (BoundConst 0) img
	  [stencil2|	 1  2  1
			 0  0  0
			-1 -2 -1 |] 



