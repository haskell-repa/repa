{-# LANGUAGE TypeOperators, PatternGuards #-}

module FFT
	(Mode(..), fft2d)
where
import Data.Array.Repa.Algorithms.DFT.Roots
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa			as A

data Mode
	= Forward
	| Reverse
	| Inverse
	deriving (Show, Eq)

-- Matrix Transform -------------------------------------------------------------------------------
-- | Compute the DFT of a square matrix.
--   If the matrix is not square then `error`.
fft2d 	:: Mode
	-> Array DIM2 Complex
	-> Array DIM2 Complex

fft2d mode arr
	| Z :. height :. width	<- extent arr
   	= if height /= width
		then error $ "fft2d: height of matrix (" ++ show height ++ ")"
			 ++  " does not match width (" ++ show width  ++ ")"
			
		else fft2d' mode arr width height
		
fft2d' mode arr width height
 = let	rofu	= case mode of
			Forward	-> force $ calcRootsOfUnity (extent arr)
			Reverse	-> force $ calcInverseRootsOfUnity (extent arr)
			Inverse	-> force $ calcInverseRootsOfUnity (extent arr)

	arr'	= fftTrans rofu $ fftTrans rofu (force arr)
		
   in	arr' `deepSeqArray` 
	case mode of
		Forward	-> force $ arr'
		Reverse	-> force $ arr'
		Inverse	-> let scale = fromIntegral (width * height) 
			   in  force $ A.map (/ scale) $ arr'

fftTrans 
	:: Array DIM2 Complex 
	-> Array DIM2 Complex 
	-> Array DIM2 Complex

{-# NOINLINE fftTrans #-}
fftTrans rofu arr'@Manifest{}
 = arr' `deepSeqArray` 
   let 	(dim :. len)	= extent arr'
   in	force $ transpose $ fft1D rofu arr'


fft1D 	:: Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex
	
{-# INLINE fft1D #-}
fft1D rofu vec
 	| n > 2	= (left +^ right) +:+ (left -^ right)
 	| n == 2	= traverse vec id swivel
 	where
		sh :. n	= extent vec

		swivel f (ix :. 0)	= f (ix :. 0) + f (ix :. 1)
		swivel f (ix :. 1)	= f (ix :. 0) - f (ix :. 1)
		
		rofu'	= evenHalf rofu
		left	= force .             fft1D rofu' . evenHalf $ vec
		right	= force . (*^ rofu) . fft1D rofu' . oddHalf  $ vec
				

halve 	:: Shape sh
	=> (sh :. Int -> sh :. Int)
	-> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex

{-# INLINE halve #-}	
halve sel arr
	= backpermute (sh :. n `div` 2) sel arr
 	where	sh :. n	= extent arr


evenHalf, oddHalf 
	:: Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex

{-# INLINE evenHalf #-}
evenHalf = halve (\(ix :. i) -> ix :. 2 * i)

{-# INLINE oddHalf #-}
oddHalf	 = halve (\(ix :. i) -> ix :. 2 * i + 1)

