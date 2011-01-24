{-# LANGUAGE BangPatterns #-}
module SolverStencil
	(solveLaplace)
where	
import Data.Array.Repa		as A
import Data.Array.Repa.Stencil	as A
import qualified Data.Array.Repa.Shape	as S

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
	      arrInit@(Manifest shInit vecInit)

 = arrBoundMask `deepSeqArray` arrBoundValue `deepSeqArray` arrInit `deepSeqArray` 
   goSolve steps shInit vecInit
	
 where	-- NOTE: We manually unpack the current array into its shape and vector to
	--	 stop GHC from unboxing the vector again for every loop. deepSeqing
	--	 the arrays at the start of solveLaplace makes the unboxings happen
	--	 at that point in the corresponding core code.
	goSolve !i !shCurrent !vecCurrent
	 = let	!arrCurrent	= Manifest shCurrent vecCurrent
	   in   if i == 0 
		 then arrCurrent
		 else let Manifest shNew vecNew
		 		= forceBlockwise
				$ relaxLaplace arrBoundMask arrBoundValue arrCurrent
		      in  goSolve (i - 1) shNew vecNew


-- | Perform one step of the relaxation for the Laplace equation.
relaxLaplace 
	:: Array DIM2 Double	-- ^ Boundary condition mask.
	-> Array DIM2 Double	-- ^ Boundary condition values.
	-> Array DIM2 Double	-- ^ Initial matrix.
	-> Array DIM2 Double

{-# INLINE relaxLaplace #-}
relaxLaplace arrBoundMask@Manifest{} arrBoundValue@Manifest{} arr@Manifest{} 
	-- apply boundary conditions
	= A.zipWith (+) arrBoundValue
	$ A.zipWith (*) arrBoundMask
	
	-- apply stencil 
	$ A.map (/ 4) (mapStencil2 stencilLaplace (BoundConst 0) arr)


-- | Stencil function for Laplace equation.
stencilLaplace :: (Elt a, Num a) => Stencil DIM2 a a
{-# INLINE stencilLaplace #-}
stencilLaplace
 = makeConvolution (Z :. 3 :. 3)
 $ \ix -> case ix of
		Z :.  0  :.  1	-> Just 1
		Z :.  0  :. -1	-> Just 1
		Z :.  1  :.  0	-> Just 1
		Z :. -1  :.  0	-> Just 1
		_		-> Nothing

