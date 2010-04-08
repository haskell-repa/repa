
module SolveArray 
	( solve
	, relaxLaplace_shift
	, relaxLaplace_backpermute)
where
import qualified Data.Array.Parallel.Unlifted 	as U
import Array					as A
import Prelude					as P


-- | Solver loop.
solve 	:: (Array DIM2 Double -> Array DIM2 Double)	-- ^ Relaxation fn to use.
	-> Int 						-- ^ Number of steps to use.
	-> Array DIM2 Double				-- ^ Boundary condition mask
	-> Array DIM2 Double				-- ^ Boundary condition value.
	-> Array DIM2 Double 				-- ^ Initial matrix.
	-> Array DIM2 Double
	
solve relaxFn steps arrBoundMask arrBoundValue arr
	| steps == 0	= arr

	| otherwise
	= solve relaxFn (steps - 1) arrBoundMask arrBoundValue
	$ applyBoundary arrBoundMask arrBoundValue
	$ relaxFn arr


-- | Perform matrix relaxation for the Laplace equation, using shift.
--   Computation fn is
--	u'(i,j) = (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1)) / 4
--
relaxLaplace_shift 
	:: Array DIM2 Double 
	-> Array DIM2 Double

{-# INLINE relaxLaplace_shift #-}
relaxLaplace_shift arr
  = A.map (/ 4)
	(A.zipWith (+)	
		(A.zipWith (+) shiftu shiftl)
		(A.zipWith (+) shiftd shiftr))

  where	shiftu = shift arr 0 ((():. 1   :.0)	  :: DIM2)
	shiftd = shift arr 0 ((():.(-1) :.0)	  :: DIM2)
	shiftl = shift arr 0 ((():. 0   :.1)	  :: DIM2)
 	shiftr = shift arr 0 ((():. 0   :.(-1)) :: DIM2)


-- | Perform matrix relaxation for the Laplce equation,
--	calling backpermute directly.
relaxLaplace_backpermute 
	:: Array DIM2 Double 
	-> Array DIM2 Double

{-# INLINE relaxLaplace_backpermute #-}
relaxLaplace_backpermute arr 
  = A.map (/ 4)
	(A.zipWith (+) 
		(A.zipWith (+) shiftu shiftl) 
		(A.zipWith (+) shiftr shiftd))
 
 where	s@((() :. n) :. m) = arrayShape arr
	shiftu = backpermuteDft arr 0 s  fu 
	shiftd = backpermuteDft arr 0 s  fd 
	shiftl = backpermuteDft arr 0 s  fl 
	shiftr = backpermuteDft arr 0 s  fr 
	fu = \((() :. i) :. j) -> if (i < (n-1)) then Just (() :. (i+1) :. j) else Nothing
	fd = \((() :. i) :. j) -> if (i > 0)     then Just (() :. (i-1) :. j) else Nothing
	fl = \((() :. i) :. j) -> if (j < (m-1)) then Just (() :. i :. (j+1)) else Nothing
	fr = \((() :. i) :. j) -> if (j > 0)     then Just (() :. i :. (j-1)) else Nothing
			

-- | Apply the boundary conditions to this matrix.
--	The mask  matrix has 0 in places where boundary conditions hold
--	and 1 otherwise.
--
--	The value matrix has the boundary condition value in places where it holds,
--	and 0 otherwise.
-- 
applyBoundary
	:: Array DIM2 Double		-- ^ boundary condition mask
	-> Array DIM2 Double		-- ^ boundary condition values
	-> Array DIM2 Double		-- ^ initial matrix
	-> Array DIM2 Double		-- ^ matrix with boundary conditions applied

{-# INLINE applyBoundary #-}
applyBoundary arrBoundMask arrBoundValue arr
 	= A.zipWith (+) arrBoundValue
	$ A.zipWith (*) arrBoundMask  arr


