{-# LANGUAGE BangPatterns #-}

module SolveCArray
	( solveLaplace_stencil
	, relaxLaplace_stencil)
where
import qualified Data.Array.Parallel.Unlifted 	as U
import Prelude					as P	hiding (zipWith)
import CArray					as CA
import Array					(Array, DIM2, (:.)(..))


-- | Version of the Laplace solver that calls the relaxation and boundary functions
--	directly, instead of them being passed in as parameters.
solveLaplace_stencil
	:: Int
	-> Array DIM2 Double
	-> Array DIM2 Double
	-> Array DIM2 Double
	-> Array DIM2 Double

solveLaplace_stencil steps arrBoundMask arrBoundValue arr
	= fromCArray
	$ solveLaplace_stencil' 
		steps
		(toCArray arrBoundMask)
		(toCArray arrBoundValue)
		(toCArray arr)


-- | Solver for the Laplace equation.
--
solveLaplace_stencil'
	:: Int
	-> CArray DIM2 Double
	-> CArray DIM2 Double
	-> CArray DIM2 Double
	-> CArray DIM2 Double

solveLaplace_stencil' steps !arrBoundMask !arrBoundValue arr
 = go steps arr
 where	go s !arr
          | s == 0    = arr
          | otherwise = go (s-1)
		$! forceCArray
			(applyBoundary arrBoundMask arrBoundValue
			$  relaxLaplace_stencil  arr)


-- | Perform matrix relaxation for the Laplace equation,
--	using a stencil function.
--
--   Computation fn is
--	u'(i,j) = (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1)) / 4
--
relaxLaplace_stencil
	:: CArray DIM2 Double
	-> CArray DIM2 Double

{-# INLINE relaxLaplace_stencil #-}
relaxLaplace_stencil !arr
 = traverseCArray arr id elemFn
 where
	_ :. height :. width	
		= carrayShape arr

	{-# INLINE elemFn #-}
	elemFn get d@(sh :. i :. j)
	 = if isBorder i j
		 then  get d
		 else (get (sh :. (i-1) :. j)
		   +   get (sh :. i     :. (j-1))
		   +   get (sh :. (i+1) :. j)
	 	   +   get (sh :. i     :. (j+1))) / 4

	-- Check if this element is on the border of the matrix.
	-- If so we can't apply the stencil because we don't have all the neighbours.
	{-# INLINE isBorder #-}
	isBorder i j
	 	=  (i == 0) || (i >= width  - 1) 
	 	|| (j == 0) || (j >= height - 1) 


-- | Apply the boundary conditions to this matrix.
--	The mask  matrix has 0 in places where boundary conditions hold
--	and 1 otherwise.
--
--	The value matrix has the boundary condition value in places where it holds,
--	and 0 otherwise.
-- 
applyBoundary
	:: CArray DIM2 Double		-- ^ boundary condition mask
	-> CArray DIM2 Double		-- ^ boundary condition values
	-> CArray DIM2 Double		-- ^ initial matrix
	-> CArray DIM2 Double		-- ^ matrix with boundary conditions applied

{-# INLINE applyBoundary #-}
applyBoundary arrBoundMask arrBoundValue arr
 	= CA.zipWith (+) arrBoundValue
	$ CA.zipWith (*) arrBoundMask  arr


