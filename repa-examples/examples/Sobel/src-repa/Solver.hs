{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module Solver 
	( Image
	, gradientX
	, gradientY )
where
import Data.Array.Repa 			as Repa
import Data.Array.Repa.Stencil

type Image	= Array DIM2 Float


gradientX :: Image -> Image
{-# NOINLINE gradientX #-}
gradientX img
 	= img `deepSeqArray` force2
 	$ forStencil2 BoundClamp img
	  [stencil2|	-1  0  1
			-2  0  2
			-1  0  1 |]


gradientY :: Image -> Image
{-# NOINLINE gradientY #-}
gradientY img
	= img `deepSeqArray` force2
	$ forStencil2 BoundClamp img
	  [stencil2|	 1  2  1
			 0  0  0
			-1 -2 -1 |] 



