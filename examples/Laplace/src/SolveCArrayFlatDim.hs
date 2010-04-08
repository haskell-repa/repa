{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE BangPatterns #-}

module SolveCArrayFlatDim
	(solveLaplace_stencil)
where
import Data.Array.Parallel.Unlifted 	((:*:)(..))
import Prelude				as P	hiding (zipWith)
import CArrayFlatDim
import Array				(Array(..), (:.)(..))
import qualified Array			as A
import FlatDim
import GHC.Exts
import GHC.Prim


-- | Version of the Laplace solver that calls the relaxation and boundary functions
--	directly, instead of them being passed in as parameters.
solveLaplace_stencil
	:: Int				-- ^ Number of iterations to solve for.
	-> A.Array A.DIM2 Double	-- ^ Mask for boundary conditions.
	-> A.Array A.DIM2 Double	-- ^ Values for boundary conditions.
	-> A.Array A.DIM2 Double	-- ^ Initial matrix.
	-> A.Array A.DIM2 Double

solveLaplace_stencil steps arrBoundMask arrBoundValue arr
 	= unflattenDimOfArray . takeArrayOfCArray
	$ solveLaplace_stencil' 
		steps
		(takeCArrayOfArray . flattenDimOfArray $ arrBoundMask)
		(takeCArrayOfArray . flattenDimOfArray $ arrBoundValue)
		(takeCArrayOfArray . flattenDimOfArray $ arr)


-- | Flatten the representation of the dimentionality of this array.
flattenDimOfArray :: A.Array A.DIM2 Double -> Array FDIM2 Double
{-# INLINE flattenDimOfArray #-}
flattenDimOfArray arr
	= Array
	{ arrayShape	= FDIM2 m n
	, arrayData	= A.arrayData arr }
	
	where	!(_ :. (I# m) :. (I# n))
			= A.arrayShape arr


-- | Unflatten the representation of the dimentionality of this array.
unflattenDimOfArray :: Array FDIM2 Double -> A.Array A.DIM2 Double
{-# INLINE unflattenDimOfArray #-}
unflattenDimOfArray farr
	= A.Array
	{ A.arrayShape	= () :. (I# m) :. (I# n)
	, A.arrayData	= arrayData farr }
	
	where	!(FDIM2 m n)	
			= arrayShape farr
	

-- | Solver for the Laplace equation.
solveLaplace_stencil'
	:: Int				-- ^ Number of iterations to solve for.
	-> CArray FDIM2 Double		-- ^ Mask for boundary conditions.
	-> CArray FDIM2 Double		-- ^ Values for boundary conditions.
	-> CArray FDIM2 Double		-- ^ Intial matrix.
	-> CArray FDIM2 Double

solveLaplace_stencil' steps !arrBoundMask !arrBoundValue arr
 = go steps arr
 where	go s !arr
	 = (if s == 0 then id else go (s - 1))
		$! (forceCArray
		$  applyBoundary arrBoundMask arrBoundValue
		$  relaxLaplace_stencil arr)


-- | Perform matrix relaxation for the Laplace equation,
--	using a stencil function.
--
--   Computation fn is
--	u'(i,j) = (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1)) / 4
--
relaxLaplace_stencil
	:: CArray FDIM2 Double
	-> CArray FDIM2 Double

{-# INLINE relaxLaplace_stencil #-}
relaxLaplace_stencil arr@(CArray shape@(FDIM2 m n) _)
 = CArray shape
 $ Left 
	(\d@(FDIM2 i j)
	  -> if isBorder d
	     	then arr !: d
	     	else (arr !: (FDIM2 (i -# 1#)  j)
		   +  arr !: (FDIM2  i        (j -# 1#))
		   +  arr !: (FDIM2 (i +# 1#)  j)
		   +  arr !: (FDIM2  i        (j +# 1#))) / 4)

 where
	isBorder :: FDIM2 -> Bool
	isBorder  (FDIM2 i j) 
		=  (i ==# 0#) || (i >=# (m -# 1#) )
		|| (j ==# 0#) || (j >=# (n -# 1#)) 


-- | Apply the boundary conditions to this matrix.
--	The mask  matrix has 0 in places where boundary conditions hold
--	and 1 otherwise.
--
--	The value matrix has the boundary condition value in places where it holds,
--	and 0 otherwise.
-- 
applyBoundary
	:: CArray FDIM2 Double		-- ^ Mask for boundary conditions.
	-> CArray FDIM2 Double		-- ^ Values for boundary conditions.
	-> CArray FDIM2 Double		-- ^ Initial matrix.
	-> CArray FDIM2 Double		-- ^ Matrix with boundary conditions applied.

{-# INLINE applyBoundary #-}
applyBoundary arrBoundMask arrBoundValue arr
 	= zipWith (+) arrBoundValue
	$ zipWith (*) arrBoundMask  arr

