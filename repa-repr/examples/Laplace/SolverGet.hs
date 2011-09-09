{-# LANGUAGE BangPatterns #-}
module SolverGet
	(solveLaplace)
where	
import Data.Array.Repa		        as R
import qualified Data.Array.Repa.Shape	as S

-- | Solver for the Laplace equation.
solveLaplace
	:: Int			-- ^ Number of iterations to use.
	-> Array U DIM2 Double	-- ^ Boundary value mask.
	-> Array U DIM2 Double	-- ^ Boundary values.
	-> Array U DIM2 Double	-- ^ Initial state.
	-> Array U DIM2 Double

{-# NOINLINE solveLaplace #-}
solveLaplace !steps !arrBoundMask !arrBoundValue !arrInit
 = [arrBoundMask, arrBoundValue, arrInit] `deepSeqArrays`
   go steps arrInit
 where  go :: Int -> Array U DIM2 Double -> Array U DIM2 Double
        go !i !arr
         | i == 0    = arr
         | otherwise 
         = let arr' = relaxLaplace arrBoundMask arrBoundValue arr
           in  arr' `deepSeqArray` go (i - 1) arr'               


-- | Perform matrix relaxation for the Laplace equation,
--	using a stencil function.
--
--   Computation fn is
--	u'(i,j) = (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1)) / 4
--
--  Apply the boundary conditions to this matrix.
--	The mask  matrix has 0 in places where boundary conditions hold
--	and 1 otherwise.
--
--	The value matrix has the boundary condition value in places where it holds,
--	and 0 otherwise.
-- 
relaxLaplace
	:: Array U DIM2 Double	-- ^ Boundary condition mask
	-> Array U DIM2 Double	-- ^ Boundary condition values
	-> Array U DIM2 Double	-- ^ Initial matrix
	-> Array U DIM2 Double	

{-# INLINE relaxLaplace #-}
relaxLaplace arrBoundMask arrBoundValue arr
 = force $ (+^) arrBoundValue
         $ (*^) arrBoundMask
         $ unsafeTraverse arr id elemFn
 where
	_ :. height :. width	
		= extent arr

	{-# INLINE elemFn #-}
	elemFn !get !d@(sh :. i :. j)
	 = if isBorder i j
		 then  get d
		 else (get (sh :. (i-1) :. j)
		   +   get (sh :. i     :. (j-1))
		   +   get (sh :. (i+1) :. j)
	 	   +   get (sh :. i     :. (j+1))) / 4

	-- Check if this element is on the border of the matrix.
	-- If so we can't apply the stencil because we don't have all the neighbours.
	{-# INLINE isBorder #-}
	isBorder !i !j
	 	=  (i == 0) || (i >= width  - 1) 
	 	|| (j == 0) || (j >= height - 1) 
