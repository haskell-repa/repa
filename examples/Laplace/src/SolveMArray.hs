{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module SolveMArray
	(solveLaplace_stencil)
where
import CArray					as CA
import Prelude					as P	hiding (zipWith)
import Array					(Array, DIM2, (:.)(..))
import qualified Data.Array.Parallel.Unlifted 	as U
import qualified Array				as A
import Data.Array.MArray
import Data.Array.IO
import GHC.IO

-- | Version of the Laplace solver that uses IOArrays and the MArray interface.
solveLaplace_stencil
	:: Int
	-> Array DIM2 Double
	-> Array DIM2 Double
	-> Array DIM2 Double
	-> Array DIM2 Double

solveLaplace_stencil steps arrBoundMask arrBoundValue arrInitial
	= unsafePerformIO (solveLaplace_io steps arrBoundMask arrBoundValue arrInitial)

solveLaplace_io steps arrBoundMask arrBoundValue arrInitial
 = do	matBoundMask	<- convertArrayToMatrix arrBoundMask
	matBoundValue	<- convertArrayToMatrix arrBoundValue

	matInitial	<- convertArrayToMatrix arrInitial
	matDest		<- convertArrayToMatrix arrInitial
	
	matFinal	<- solveLaplace_matrix steps
				matBoundMask
				matBoundValue
				matInitial
				matDest
			
	elemsFinal	<- getElems $ matrixData matFinal
			
	let arrFinal	= A.Array
			{ A.arrayShape	= A.arrayShape arrInitial
			, A.arrayData	= U.fromList $ elemsFinal }
				
	return arrFinal
	
-----
solveLaplace_matrix
	:: Int				-- ^ Number of steps to run.
	-> Matrix Double		-- ^ Boundary condition mask.
	-> Matrix Double		-- ^ Boundary condition value.
	-> Matrix Double 		-- ^ Destination matrix
	-> Matrix Double		-- ^ Initial matrix
	-> IO (Matrix Double)

solveLaplace_matrix steps matBoundMask matBoundValue matDest matInitial
 = go steps matDest matInitial
 where
	go !s !matDst !matSrc
	 | s == 0	= return matSrc
	 | otherwise
	 = do	relaxLaplace matBoundMask matBoundValue matDst matSrc
		go (s - 1) matSrc matDst
		

-- | Perform matrix relaxation for the Laplace equation, 
---	using a stencil function. Also apply boundary conditions along the way.
--
--   Stencil computation fn is
--	u'(i,j) = (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1)) / 4
--
--   We only iterate over the inner elements of the matrix, avoiding
--	the boundary where the stencil does not apply.
--
relaxLaplace 
	:: Matrix Double		-- ^ Boundary value mask.
	-> Matrix Double		-- ^ Boundary values.
	-> Matrix Double		-- ^ Destination matrix.
	-> Matrix Double		-- ^ Source matrix.
	-> IO ()
	
{-# INLINE relaxLaplace #-}
relaxLaplace !matBoundMask !matBoundValue !matDest !matSrc
 = go 1 1
 where	w		= matrixWidth  matDest - 1
	h		= matrixHeight matDest - 1
	
	go !y !x
	 | x >= w	= go (y + 1) 1
	 | y >= h	= return ()
	 | otherwise
	 = do
		l	<- readMatrix matSrc y       (x - 1)
		r	<- readMatrix matSrc y       (x + 1)
		u	<- readMatrix matSrc (y + 1)  x
		d	<- readMatrix matSrc (y - 1)  x

		bm	<- readMatrix matBoundMask  y x
		bv	<- readMatrix matBoundValue y x

		let avg	= (l + r + u + d) / 4
		let val	= (avg * bm) + bv
			
		writeMatrix matDest y x val
		go y (x + 1)


-- Matrix -----------------------------------------------------------------------------------------
data Matrix e
	= Matrix
	{ matrixWidth	:: Int
	, matrixHeight	:: Int
	, matrixData	:: IOUArray Int e }
		

-- | Convert an array to an IOArray
convertArrayToMatrix
	:: (U.Elt e, MArray IOUArray e IO)
	=> Array DIM2 e
	-> IO (Matrix e)
	
convertArrayToMatrix arr
 = do	let _ :. height :. width	
		= A.arrayShape arr

	ioarr	<- newListArray 
			(0, (width * height) - 1) 
			(U.toList   $ A.arrayData  arr)
			
	return	$ Matrix
		{ matrixWidth	= width
		, matrixHeight	= height
		, matrixData	= ioarr }


-- | Write a value into a matrix
writeMatrix :: Matrix Double -> Int -> Int -> Double -> IO ()
{-# INLINE writeMatrix #-}
writeMatrix mat y x val
	= writeArray (matrixData mat) (indexMatrix mat y x) val


-- | Read a value from a matrix
readMatrix :: Matrix Double -> Int -> Int -> IO Double
{-# INLINE readMatrix #-}
readMatrix mat y x
	= readArray (matrixData mat) (indexMatrix mat y x)


indexMatrix :: Matrix Double -> Int -> Int -> Int
{-# INLINE indexMatrix #-}
indexMatrix mat y x
	= x + (y * matrixWidth mat)
	
