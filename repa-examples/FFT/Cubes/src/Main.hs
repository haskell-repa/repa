{-# LANGUAGE PatternGuards #-}

import Data.Array.Repa.Algorithms.FFT
import Data.Array.Repa.Algorithms.DFT.Center
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.ColorRamp
import Data.Array.Repa				as A
import Data.Word
import System.Environment
import Control.Monad
import Prelude					as P

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	 [size]			-> mainWithArgs (read size) Nothing
	 [size, fileOut]	-> mainWithArgs (read size) (Just fileOut)

         _ -> putStr $ unlines
		[ "Usage: repa-fft2d <size> [file.bmp]"
		, "" ]
			
			
mainWithArgs size mFileOut
 = let
	-- Generate a cube holding a checkerboard pattern.
	shape	= Z :. size :. size :. size
	checkSize	= size `div` 4
	center		= size `div` 2


	arrInit :: Array DIM3 Complex

-- 	A checkerboard pattern
{-	arrInit	
		= force 
		$ fromFunction shape 
			(\(Z :. z :. y :. x)
			 -> let	isOn	=     ((z `div` checkSize) `mod` 2 == 0)
					`xor` ((y `div` checkSize) `mod` 2 == 0)
					`xor` ((x `div` checkSize) `mod` 2 == 0)
			    in	if isOn then 1 :*: 0 else 0 :*: 0)
-}
	arrInit	
		= force 
		$ fromFunction shape 
			(\ix -> if isInCenteredCube center checkSize ix 
					then 1 :*: 0 else 0 :*: 0)

	-- Transform to frequency space.
	arrCentered	= center3d arrInit
	arrFreq		= fft3d Forward arrCentered
	
	-- Zap out the high frequency components
	cutoff		= 4
	arrFilt		= traverse arrFreq id (highpass center cutoff)
	
	-- Do the inverse transform to get back to image space.
	arrInv		= fft3d Inverse arrFilt
	arrFinal	= arrFilt
		
   in 	arrFinal `deepSeqArray`
	 do 	case mFileOut of
		 Just fileOut	-> mapM_ (dumpSlice "out" arrFinal) [0..size - 1]
		 Nothing 	-> return ()


-- | Dump a numberd slice of this array to a BMP file.
dumpSlice 
	:: FilePath
	-> Array DIM3 Complex
	-> Int
	-> IO ()

dumpSlice prefix arr sliceNum
 = do	let arrSlice	= slice arr (Any :. sliceNum :. All)
	let arrGrey	= A.map (truncate . (* 255) . mag) arrSlice
	let fileName	= prefix ++ (pad0 3 (show sliceNum)) ++ ".bmp"

	writeComponentsToBMP fileName
		arrGrey arrGrey arrGrey

pad0 n str
 = P.replicate  (n - length str) '0' ++ str


xor a b
 = case (a, b) of
	(True, True)	-> False
	(True, False)	-> True
	(False, True)	-> True
	(False, False)	-> False


{-# INLINE isInCenteredCube #-}
isInCenteredCube center cutoff ix@(_ :. z :. y :. x)
 = let	high	= center + cutoff
	low	= center - cutoff
   in	x >= low && x <= high
     && y >= low && y <= high
     && z >= low && z <= high


{-# INLINE highpass #-}
highpass center cutoff get ix
	| isInCenteredCube center cutoff ix	= 0
	| otherwise				= get ix

{-# INLINE lowpass #-}
lowpass center cutoff get ix
	| isInCenteredCube center cutoff ix	= get ix
	| otherwise				= 0


