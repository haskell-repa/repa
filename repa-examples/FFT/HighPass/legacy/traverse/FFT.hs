
-- | Compute the DFT of a vector.
fft1d 	:: Array DIM1 Complex
	-> Array DIM1 Complex
	
fft1d vec
 = let	(Z :. len)	= extent vec
   in	fft1d' len 0 1 vec


fft1d' 	:: Int 			-- ^ Length.
	-> Int			-- ^ Offset.
	-> Int 			-- ^ Stride.
	-> Array DIM1 Complex
	-> Array DIM1 Complex

fft1d' n' offset' stride' vec@Manifest{}
 = go n' offset' stride'
 where	go !n !offset !stride
	 | n == 1
	 = fromFunction (Z :. 1) (\_ -> vec !: (Z :. offset))
	
	 | otherwise
	 = let	n2	= n `div` 2
		evenT	= force $ go n2 offset            (stride * 2) 
		oddT	= force $ go n2 (offset + stride) (stride * 2)
		
		left	= traverse2 evenT oddT
				(\sh _ -> sh) 
				(\getEven getOdd k@(_ :. k') -> getEven k + twiddle k' n * getOdd k)
		
		right	= traverse2 evenT oddT
				(\sh _ -> sh)
				(\getEven getOdd k@(_ :. k') -> getEven k - twiddle k' n * getOdd k)
				
	   in	left +:+ right


twiddle :: Int 			-- ^ Index.
	-> Int 			-- ^ Length of vector.
	-> Complex

{-# INLINE twiddle #-}
twiddle !k' !n'
 	=  cos (2 * pi * k / n) :*: (- sin  (2 * pi * k / n))
	where 	k	= fromIntegral k'
		n	= fromIntegral n'
