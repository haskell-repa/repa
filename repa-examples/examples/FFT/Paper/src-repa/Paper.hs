{-# LANGUAGE TypeOperators #-}

module Paper where
	
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa			as A

-- | Take the odd or even halves of an array.
evenHalf, oddHalf
	:: Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex
	
evenHalf = halve (\(ix :. i) -> ix :. 2*i)
oddHalf  = halve (\(ix :. i) -> ix :. 2*i+1)

halve 	:: Shape sh
	=> (sh :. Int -> sh :. Int)
	-> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex
	
halve sel arr
 = backpermute (sh :. n `div` 2) sel arr
 where	sh :. n = extent arr


fft2d	:: Array DIM2 Complex
	-> Array DIM2 Complex
	-> Array DIM2 Complex

fft2d rofu  
	= fftTrans . fftTrans
	where fftTrans = transpose . fft1d rofu
	

-- | Rank generalised worker.
fft1d	:: Shape sh
	=> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex
	-> Array (sh :. Int) Complex

fft1d rofu v
 | n > 2	= (left +^ right) +:+ (left -^ right)
 | n == 2	= traverse v id swivel
 where
	(_ :. n) = extent v
	swivel f (ix :. 0) = f (ix :. 0) + f (ix :. 1)
	swivel f (ix :. 1) = f (ix :. 0) - f (ix :. 1)
	
	rofu'	= evenHalf rofu
	left	= force .             fft1d rofu' . evenHalf $ v
	right	= force . (*^ rofu) . fft1d rofu' . oddHalf  $ v
	
