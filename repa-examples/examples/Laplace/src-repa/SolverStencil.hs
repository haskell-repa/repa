{-# LANGUAGE BangPatterns, TemplateHaskell, QuasiQuotes #-}
module SolverStencil
	(solveLaplace)
where	
import Data.Array.Repa				as A
import Data.Array.Repa.Stencil			as A
import Data.Array.Repa.Algorithms.Iterate	as A
import qualified Data.Array.Repa.Shape		as S
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Solver for the Laplace equation.
solveLaplace
	:: Int			-- ^ Number of iterations to use.
	-> Array DIM2 Double	-- ^ Boundary value mask.
	-> Array DIM2 Double	-- ^ Boundary values.
	-> Array DIM2 Double	-- ^ Initial state.
	-> Array DIM2 Double

{-# NOINLINE solveLaplace #-}
solveLaplace !steps !arrBoundMask !arrBoundValue !arrInit
 = go steps arrInit
 where 	go 0 !arr	= arr
	go n !arr	= go (n - 1) (relaxLaplace arrBoundMask arrBoundValue arr)
	

{-# INLINE relaxLaplace #-}
relaxLaplace
	:: Array DIM2 Double	-- ^ Boundary value mask.
	-> Array DIM2 Double	-- ^ Boundary values.
	-> Array DIM2 Double	-- ^ Initial state.
	-> Array DIM2 Double

relaxLaplace 
	 arrBoundMask@(Array _ [Region RangeAll (GenManifest _)])
	arrBoundValue@(Array _ [Region RangeAll (GenManifest _)])
	          arr@(Array _ [Region RangeAll (GenManifest _)])
  = [arrBoundMask, arrBoundValue, arr] `deepSeqArrays` 
    let	ex		= extent arr
	arrBoundMask'	= reshape ex arrBoundMask
	arrBoundValue'	= reshape ex arrBoundValue
	arr'		= reshape ex arr
    in	force
	 $ A.zipWith (+) arrBoundValue'
	 $ A.zipWith (*) arrBoundMask'
	 $ A.map (/ 4)
	 $ mapStencil2 BoundClamp
	   [stencil2| 	0 1 0
			1 0 1 
			0 1 0 |] arr'
			