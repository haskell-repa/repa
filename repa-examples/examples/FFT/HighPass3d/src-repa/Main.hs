{-# LANGUAGE PatternGuards #-}

import Data.Array.Repa.Algorithms.FFT
import Data.Array.Repa.Algorithms.DFT.Center
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa.Algorithms.ColorRamp
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing
import Data.Array.Repa				as A
import qualified Data.Array.Repa.Repr.Unboxed   as U
import Data.Word
import System.Environment
import Control.Monad
import Prelude					as P

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	 [size, prefix]	-> mainWithArgs (read size) prefix

         _ -> putStr $ unlines
		[ "Usage: repa-fft3d-highpass <size::Int> <prefix>"
		, ""
		, "   Size must be a power of two."
		, "   You get a stack of prefix###.bmp files resulting from high-pass filtering a cube."
		, "" ]
			
			
mainWithArgs size prefixOut
 = do
	-- Generate a cube for initial data.
	let shape	= Z :. size :. size :. size
	let cubeSize	= size `div` 4
	let center	= size `div` 2
	let cutoff		= 4

	arrInit	<- now $ computeP
                $  fromFunction shape 
			(\ix -> if isInCenteredCube center cubeSize ix 
					then (1, 0) else (0, 0))

	(arrFinal, t) <- time $ now $ transform arrInit center cutoff
	putStr (prettyTime t)

 	mapM_ (dumpSlice prefixOut arrFinal) [0..size - 1]


-- | To the high pass transform.
transform
	:: Array U DIM3 Complex
	-> Int
	-> Int
	-> Array U DIM3 Complex
transform arrInit center cutoff
 = let	-- Transform to frequency space.
	arrCentered	= center3d arrInit
	arrFreq		= fft3d Forward arrCentered
	
	-- Zap out the high frequency components
	arrFilt		= traverse arrFreq id (highpass center cutoff)
	
	-- Do the inverse transform to get back to image space.
	arrInv		= fft3d Inverse arrFilt
   in	arrInv


-- | Dump a numbered slice of this array to a BMP file.
dumpSlice 
	:: FilePath
	-> Array U DIM3 Complex
	-> Int
	-> IO ()

dumpSlice prefix arr sliceNum
 = do	let arrSlice	= slice arr (Any :. sliceNum :. All)
	let arrGrey	= computeUnboxedP $ A.map (truncate . (* 255) . mag) arrSlice
	let fileName	= prefix P.++ (pad0 3 (show sliceNum)) P.++ ".bmp"

	writeImageToBMP fileName
	        (U.zip3 arrGrey arrGrey arrGrey)

pad0 n str
 = P.replicate  (n - length str) '0' P.++ str


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

