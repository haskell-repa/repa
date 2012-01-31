{-# LANGUAGE BangPatterns, TemplateHaskell, QuasiQuotes #-}
module SolverStencil
	(solveLaplace)
where	
import Data.Array.Repa				as A
import Data.Array.Repa.Stencil			as A
import Data.Array.Repa.Stencil.Dim2		as A
import qualified Data.Array.Repa.Shape		as S
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Solver for the Laplace equation.
solveLaplace
	:: Int			-- ^ Number of iterations to use.
	-> Array U DIM2 Double	-- ^ Boundary value mask.
	-> Array U DIM2 Double	-- ^ Boundary values.
	-> Array U DIM2 Double	-- ^ Initial state.
	-> Array U DIM2 Double

{-# NOINLINE solveLaplace #-}
solveLaplace !steps !arrBoundMask !arrBoundValue !arrInit
 = go steps arrInit
 where 	go 0 !arr	= arr
	go n !arr	= go (n - 1) (relaxLaplace arrBoundMask arrBoundValue arr)
	

{-# INLINE relaxLaplace #-}
relaxLaplace
	:: Array U DIM2 Double	-- ^ Boundary value mask.
	-> Array U DIM2 Double	-- ^ Boundary values.
	-> Array U DIM2 Double	-- ^ Initial state.
	-> Array U DIM2 Double

relaxLaplace arrBoundMask arrBoundValue arr
  = [arrBoundMask, arrBoundValue, arr] `deepSeqArrays` 
    let	ex		= extent arr
	arrBoundMask'	= reshape ex arrBoundMask
	arrBoundValue'	= reshape ex arrBoundValue
	arr'		= reshape ex arr
    in	computeP
	 $ A.zipWith (+) arrBoundValue'
	 $ A.zipWith (*) arrBoundMask'
	 $ A.map (/ 4)
	 $ mapStencil2 BoundClamp
	   [stencil2| 	0 1 0
			1 0 1 
			0 1 0 |] arr'
			
