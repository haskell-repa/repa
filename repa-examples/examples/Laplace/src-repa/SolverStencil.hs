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
solveLaplace steps arrBoundMask arrBoundValue arrInit
 	= arrBoundMask `deepSeqArray` arrBoundValue `deepSeqArray` arrInit `deepSeqArray` 
	  iterateBlockwise' steps arrInit
	$ A.zipWith (+) arrBoundValue . A.zipWith (*) arrBoundMask
	. A.map (/ 4)
	. mapStencil2 (BoundConst 0) 
	  [stencil2| 	0 1 0
			1 0 1 
			0 1 0 |]
			