{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module SolverY
	( gradientY_only )
where
import Data.Array.Repa 			as Repa
import Data.Array.Repa.Stencil
import Solver

gradientY_only :: Image -> Image
{-# NOINLINE gradientY_only #-}
gradientY_only img@Manifest{}
	= img `deepSeqArray` forceBlockwise 
	$ forStencil2 BoundClamp img
	  [stencil2|	 1  2  1
			 0  0  0
			-1 -2 -1 |] 
