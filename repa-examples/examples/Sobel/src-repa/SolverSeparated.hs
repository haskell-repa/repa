{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module SolverSeparated
	( Image
	, gradientX
	, gradientY )
where
import Data.Array.Repa 			as Repa
import Data.Array.Repa.Stencil

type Image	= Array DIM2 Float

-- Separated version
gradientX :: Image -> Image
gradientX = gradientX1 -- . gradientX2

gradientX1 :: Image -> Image
{-# NOINLINE gradientX1 #-}
gradientX1 img@Manifest{}
	= img `deepSeqArray` forceBlockwise
	$ forStencil2 BoundClamp img
	  [stencil2|    1 0 -1 |]

gradientX2 :: Image -> Image
{-# NOINLINE gradientX2 #-}
gradientX2 img@Manifest{}
	= img `deepSeqArray` forceBlockwise
	$ forStencil2 BoundClamp img
	  [stencil2|    1
	                2
	                1 |]


gradientY :: Image -> Image
gradientY = gradientY1 -- . gradientY2

gradientY1 :: Image -> Image
{-# NOINLINE gradientY1 #-}
gradientY1 img@Manifest{}
	= img `deepSeqArray` forceBlockwise
	$ forStencil2 BoundClamp img
	  [stencil2|    1 2 1 |]

gradientY2 :: Image -> Image
{-# NOINLINE gradientY2 #-}
gradientY2 img@Manifest{}
	= img `deepSeqArray` forceBlockwise
	$ forStencil2 BoundClamp img
	  [stencil2|     1
	                 0
	                -1 |]
