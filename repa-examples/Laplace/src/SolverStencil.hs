{-# LANGUAGE BangPatterns #-}
module SolverStencil
	(solveLaplace)
where	
import Data.Array.Repa				as A
import Data.Array.Repa.Stencil			as A
import Data.Array.Repa.Algorithms.Iterate	as A
import qualified Data.Array.Repa.Shape		as S

-- | Solver for the Laplace equation.
solveLaplace
	:: Int			-- ^ Number of iterations to use.
	-> Array DIM2 Double	-- ^ Boundary value mask.
	-> Array DIM2 Double	-- ^ Boundary values.
	-> Array DIM2 Double	-- ^ Initial state.
	-> Array DIM2 Double

{-# NOINLINE solveLaplace #-}
solveLaplace steps
	 arrBoundMask@Manifest{}
	arrBoundValue@Manifest{}
	      arrInit@Manifest{}

 = arrBoundMask `deepSeqArray` arrBoundValue `deepSeqArray` arrInit `deepSeqArray` 
   let	stencil	= makeStencil (Z :. 3 :. 3)
 		$ \ix -> case ix of
			Z :.  0  :.  1	-> Just 1
			Z :.  0  :. -1	-> Just 1
			Z :.  1  :.  0	-> Just 1
			Z :. -1  :.  0	-> Just 1
			_		-> Nothing

	relax arr
		= A.zipWith (+) arrBoundValue
		$ A.zipWith (*) arrBoundMask
		$ A.map (/ 4) (mapStencil2 stencil (BoundConst 0) arr)

   in	iterateBlockwise steps relax arrInit

