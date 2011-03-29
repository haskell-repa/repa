{-# LANGUAGE BangPatterns, PackageImports #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}
module Data.Array.Repa.Algorithms.Iterate
	(iterateBlockwise, iterateBlockwise')
where
import Data.Array.Repa

-- | Iterate array transformation function a fixed number of times, applying `force2` between
--   each iteration.
iterateBlockwise
	:: (Elt a, Num a)
	=> Int					-- ^ Number of iterations to run for.
	-> (Array DIM2 a -> Array DIM2 a)	-- ^ Fn to step the array.
	-> Array DIM2 a				-- ^ Initial array value.
	-> Array DIM2 a
	
{-# INLINE iterateBlockwise #-}
iterateBlockwise steps f arrInit@(Array shInit [Region RangeAll (GenManifest vecInit)])
 = arrInit `deepSeqArray`
  goSolve steps shInit vecInit

 where	-- NOTE: We manually unpack the current array into its shape and vector to
	--	 stop GHC from unboxing the vector again for every loop. deepSeqing
	--	 the arrays at the start of solveLaplace makes the unboxings happen
	--	 at that point in the corresponding core code.
	goSolve !i !shCurrent !vecCurrent
	 = let	!arrCurrent	= fromVector shCurrent vecCurrent
	   in   if i == 0 
		 then arrCurrent
		 else let arrNew@(Array _ [Region RangeAll (GenManifest _)]) 
				= force2 $ f arrCurrent
		      in  goSolve (i - 1) (extent arrNew) (toVector arrNew)


-- | As above, but with the parameters flipped.
iterateBlockwise'
	:: (Elt a, Num a)
	=> Int					-- ^ Number of iterations to run for.
	-> Array DIM2 a				-- ^ Initial array value.
	-> (Array DIM2 a -> Array DIM2 a)	-- ^ Fn to step the array.
	-> Array DIM2 a

{-# INLINE iterateBlockwise' #-}
iterateBlockwise' steps arr fn
	= iterateBlockwise steps fn arr