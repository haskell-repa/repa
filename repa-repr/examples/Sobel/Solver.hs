{-# LANGUAGE QuasiQuotes, MagicHash #-}
module Solver
	( Image
	, gradientX)
--	, gradientY )
where
import Data.Array.Repa 			as R
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import GHC.Integer
import GHC.Exts

type Image	= Array U DIM2 Float

mkZero :: () -> Float#
mkZero _ = case 0.0 of
                F# f -> f

{-# RULES "floatFromInteger" floatFromInteger 0 = mkZero () #-}

gradientX :: Image -> Image
{-# NOINLINE gradientX #-}
gradientX img
 	= img `deepSeqArray` force
 	$ forStencil2 (BoundConst 0) img
	  [stencil2|	-1  0  1
			-2  0  2
			-1  0  1 |]

{-
gradientY :: Image -> Image
{-# NOINLINE gradientY #-}
gradientY img
	= img `deepSeqArray` force
	$ forStencil2 (BoundConst 0) img
	  [stencil2|	 1  2  1
			 0  0  0
			-1 -2 -1 |] 
-}