{-# LANGUAGE TypeOperators, PatternGuards, RankNTypes, ScopedTypeVariables, BangPatterns, FlexibleContexts #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

-- | Fast computation of Discrete Fourier Transforms using the Cooley-Tuckey algorithm.
--
--   Time complexity is O(n log n) in the size of the input.
--
module Data.Array.Repa.Algorithms.FFT
	( Mode(..)
	, fft3d
	, fft2d
	, fft1d)
where
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa				as A

data Mode
	= Forward
	| Reverse
	| Inverse
	deriving (Show, Eq)

{-# INLINE signOfMode #-}
signOfMode :: Mode -> Double
signOfMode mode
 = case mode of
	Forward		-> (-1)
	Reverse		->   1
	Inverse		->   1


-- 3D Transform -----------------------------------------------------------------------------------
-- | Compute the DFT of a 3d array.
fft3d 	:: Mode
	-> Array DIM3 Complex
	-> Array DIM3 Complex

fft3d mode arr
 = let	_ :. depth :. height :. width	= extent arr
	sign	= signOfMode mode
	scale 	= fromIntegral (depth * width * height) 
		
   in	arr `deepSeqArray` 
	case mode of
		Forward	-> fftTrans3d sign $ fftTrans3d sign $ fftTrans3d sign arr
		Reverse	-> fftTrans3d sign $ fftTrans3d sign $ fftTrans3d sign arr
		Inverse	-> force $ A.map (/ scale) 
				$ fftTrans3d sign $ fftTrans3d sign $ fftTrans3d sign arr

fftTrans3d 
	:: Double
	-> Array DIM3 Complex 
	-> Array DIM3 Complex

{-# NOINLINE fftTrans3d #-}
fftTrans3d sign arr'
 = let 	arr		= force arr'
	(sh :. len)	= extent arr
   in	force $ rotate3d $ fft sign sh len arr


rotate3d :: Array DIM3 Complex -> Array DIM3 Complex
{-# INLINE rotate3d #-}
rotate3d arr
 = backpermute (sh :. m :. k :. l) f arr
 where	(sh :. k :. l :. m)		= extent arr
	f (sh' :. m' :. k' :. l')	= sh' :. k' :. l' :. m'



-- Matrix Transform -------------------------------------------------------------------------------
-- | Compute the DFT of a matrix.
fft2d 	:: Mode
	-> Array DIM2 Complex
	-> Array DIM2 Complex

fft2d mode arr
 = let	_ :. height :. width	= extent arr
	sign	= signOfMode mode
	scale 	= fromIntegral (width * height) 
		
   in	arr `deepSeqArray` 
	case mode of
		Forward	-> fftTrans2d sign $ fftTrans2d sign arr
		Reverse	-> fftTrans2d sign $ fftTrans2d sign arr
		Inverse	-> force $ A.map (/ scale) $ fftTrans2d sign $ fftTrans2d sign arr

fftTrans2d 
	:: Double
	-> Array DIM2 Complex 
	-> Array DIM2 Complex

{-# NOINLINE fftTrans2d #-}
fftTrans2d sign arr'
 = let 	arr		= force arr'
	(sh :. len)	= extent arr
   in	force $ transpose $ fft sign sh len arr


-- Vector Transform -------------------------------------------------------------------------------
-- | Compute the DFT of a vector.
fft1d	:: Mode 
	-> Array DIM1 Complex 
	-> Array DIM1 Complex
	
fft1d mode arr
 = let	_ :. len	= extent arr
	sign	= signOfMode mode
	scale	= fromIntegral len
	
   in	arr `deepSeqArray`
	case mode of
		Forward	-> fftTrans1d sign arr
		Reverse	-> fftTrans1d sign arr
		Inverse -> force $ A.map (/ scale) $ fftTrans1d sign arr

fftTrans1d
	:: Double 
	-> Array DIM1 Complex
	-> Array DIM1 Complex

{-# NOINLINE fftTrans1d #-}
fftTrans1d sign arr'
 = let	arr		= force arr'
	(sh :. len)	= extent arr
   in	fft sign sh len arr


-- Rank Generalised Worker ------------------------------------------------------------------------
{-# INLINE fft #-}
fft !sign !sh !lenVec !vec
 = go lenVec 0 1
 where	go !len !offset !stride
	 | len == 2
	 = force $ fromFunction (sh :. 2) swivel
	
	 | otherwise
	 = combine len 
		(go (len `div` 2) offset            (stride * 2))
		(go (len `div` 2) (offset + stride) (stride * 2))

	 where	swivel (sh' :. ix)
		 = case ix of
			0	-> vec !: (sh' :. offset) + vec !: (sh' :. (offset + stride))
			1	-> vec !: (sh' :. offset) - vec !: (sh' :. (offset + stride))

		{-# INLINE combine #-}
		combine !len' evens@Manifest{} odds@Manifest{}
 	 	 = evens `deepSeqArray` odds `deepSeqArray`
   	   	   let	odds'	= traverse odds id (\get ix@(_ :. k) -> twiddle sign k len' * get ix) 
   	   	   in	force 	$ (evens +^ odds') +:+ (evens -^ odds')


-- Compute a twiddle factor.
twiddle :: Double
	-> Int 			-- index
	-> Int 			-- length
	-> Complex

{-# INLINE twiddle #-}
twiddle sign k' n'
 	=  (cos (2 * pi * k / n), sign * sin  (2 * pi * k / n))
	where 	k	= fromIntegral k'
		n	= fromIntegral n'
      

