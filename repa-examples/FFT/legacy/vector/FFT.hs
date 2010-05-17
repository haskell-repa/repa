{-# LANGUAGE BangPatterns #-}

-- | A version of FFT using unboxed vectors.
module FFT
	( Mode(..)
	, fft2d
	, fft)
where
	
import qualified Data.Vector.Unboxed	as U
import qualified Data.Vector		as V
import Data.Vector			(Vector)
import Data.Complex
import Debug.Trace

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
		

-- | Compute the 2D fft of a complex matrix.
fft2d 	:: Mode 
	-> Int 
	-> Int 
	-> U.Vector (Complex Double) 
	-> U.Vector (Complex Double)

{-# INLINE fft2d #-}
fft2d mode width height arr
 = let	sign	= signOfMode mode	

	arr'	= transpose width height
		$ V.generate height 
			(\r -> fftWithSignOffset sign width (r * width) arr)
		
	arr''	= transpose height width
		$ V.generate width
			(\c -> fftWithSignOffset sign width (c * height) arr')
	
   in	case mode of
		Forward	-> arr''
		Reverse	-> arr''
		Inverse	-> U.map (/ (fromIntegral (width * height))) arr''
	
	
-- | Transpose a matrix expressed as a vector of vectors in to 
--   a single flat vector that contains all the elements.
transpose 
	:: Int	-- width 
	-> Int	-- height
	-> Vector (U.Vector (Complex Double)) 
	-> U.Vector (Complex Double)

{-# INLINE transpose #-}
transpose srcWidth srcHeight rows
 = rows `seq` 
   let	dstWidth	= srcHeight
	dstHeight	= srcWidth

	elemFn ix
 	 = let	x	= ix `rem` dstWidth
		y	= ix `div` dstWidth
	   in	(rows `V.unsafeIndex` x) `U.unsafeIndex` y

   in	U.generate (dstWidth * dstHeight) elemFn


-- | Compute the Discrete Fourier Transform (DFT) of a complex vector.
fft :: Mode -> U.Vector (Complex Double) -> U.Vector (Complex Double)
fft mode vec
 = let	len	= U.length vec
   in case mode of
	Forward	-> fftWithSignOffset (signOfMode mode) len 0 vec
	Reverse	-> fftWithSignOffset (signOfMode mode) len 0 vec
	Inverse	
	 -> let 
	    in	U.map (/ (fromIntegral len)) 
			$ fftWithSignOffset (signOfMode mode) len 0 vec


-- | The main 1d FFT kernel.
fftWithSignOffset :: Double -> Int -> Int -> U.Vector (Complex Double) -> U.Vector (Complex Double)
{-# INLINE fftWithSignOffset #-}
fftWithSignOffset !sign !length' !offset' !vec
 = go length' offset' 1
 where go !n !offset !stride 

	| n == 1
	= U.singleton (vec `U.unsafeIndex` offset)

	| n == 2
	= U.generate 2 $ \ix ->
		case ix of
			0	-> vec `U.unsafeIndex` offset + vec `U.unsafeIndex` (offset + stride)
			1	-> vec `U.unsafeIndex` offset - vec `U.unsafeIndex` (offset + stride)

	| otherwise
	= let	n2		= n `div` 2
		evenT		= go n2 offset            (stride * 2)
		oddT		= go n2 (offset + stride) (stride * 2)

		leftFn k	= evenT `U.unsafeIndex` k + twiddle sign k n * oddT `U.unsafeIndex` k
		left		= U.generate n2 leftFn
			
		rightFn k	= evenT `U.unsafeIndex` k - twiddle sign k n * oddT `U.unsafeIndex` k 
		right		= U.generate n2 rightFn
		
	  in 	left U.++ right


-- Compute a twiddle factor.
twiddle :: Double
	-> Int 			-- index
	-> Int 			-- length
	-> Complex Double

{-# INLINE twiddle #-}
twiddle sign k' n'
 	=  cos (2 * pi * k / n) :+ (sign * sin  (2 * pi * k / n))
	where 	k	= fromIntegral k'
		n	= fromIntegral n'
