{-# LANGUAGE BangPatterns, PackageImports #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}
module Data.Array.Repa.Algorithms.Iterate
	(iterateBlockwise)
where
import Data.Array.Repa
import Data.Vector.Unboxed


-- | Iterate a stencil a fixed number of times, applying constant value boundary conditions after each iteration.
--   The boundary conditions are specified with a mask and value array. 
--   The mask contains 1 for indicies where the boundary conditions apply, and 0 otherwise.
iterateBlockwise
	:: (Unbox a, Num a)
	=> Int					-- ^ Number of iterations to run for.
	-> (Array DIM2 a -> Array DIM2 a)	-- ^ Fn to step the array.
	-> Array DIM2 a				-- ^ Initial array value.
	-> Array DIM2 a
	
{-# INLINE iterateBlockwise #-}
iterateBlockwise steps f arrInit@(Manifest shInit vecInit)
 = arrInit `deepSeqArray`
  goSolve steps shInit vecInit

 where	-- NOTE: We manually unpack the current array into its shape and vector to
	--	 stop GHC from unboxing the vector again for every loop. deepSeqing
	--	 the arrays at the start of solveLaplace makes the unboxings happen
	--	 at that point in the corresponding core code.
	goSolve !i !shCurrent !vecCurrent
	 = let	!arrCurrent	= Manifest shCurrent vecCurrent
	   in   if i == 0 
		 then arrCurrent
		 else let Manifest shNew vecNew = forceBlockwise $ f arrCurrent
		      in  goSolve (i - 1) shNew vecNew
