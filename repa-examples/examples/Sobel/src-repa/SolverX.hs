{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module SolverX
	(gradientX_only)
where
import Data.Array.Repa
import Data.Array.Repa.Stencil
-- import qualified Data.Array.Repa.Shape	as S
import Solver


gradientX_only :: Image -> Image
{-# NOINLINE gradientX_only #-}
gradientX_only img@(Array _ [Region RangeAll (GenManifest _)])
 	= img `deepSeqArray` 
          force2 $ forStencil2 BoundClamp img
	  [stencil2|	-1  0  1
			-2  0  2
			-1  0  1 |]

	
{-
gradientX_only :: Image -> Image
{-# NOINLINE gradientX_only #-}
gradientX_only 
	= withManifest1 $ \img -> img `deepSeqArray` 
          force2 $ forStencil2 BoundClamp img
	  [stencil2|	-1  0  1
			-2  0  2
			-1  0  1 |]


withManifest1 :: Shape sh => (Array sh a -> b) -> Array sh a -> b
{-# INLINE withManifest1 #-}
withManifest1 f arr@(Array sh [Region RangeAll (GenManifest vec)])
	= f `seq` arr `deepSeqArray` f arr
-}