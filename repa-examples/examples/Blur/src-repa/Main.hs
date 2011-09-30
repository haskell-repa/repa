{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

import Data.List
import Control.Monad
import System.Environment
import Data.Word
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing
import Data.Array.Repa 			        as A
import qualified Data.Array.Repa.Repr.Unboxed   as U
import Data.Array.Repa.Stencil		        as A
import Data.Array.Repa.Stencil.Dim2	        as A
import Prelude				        as P

main 
 = do	args	<- getArgs
	case args of
	 [iterations, fileIn, fileOut]	-> run (read iterations) fileIn fileOut
	 _				-> usage

usage 	= putStr $ unlines
	[ "repa-blur <iterations::Int> <fileIn.bmp> <fileOut.bmp>" ]


-- | Perform the blur.
run :: Int -> FilePath -> FilePath -> IO ()
run iterations fileIn fileOut
 = do	arrRGB	<- liftM (either (error . show) id) 
		$  readImageFromBMP fileIn

	arrRGB `deepSeqArray` return ()
	let (arrRed, arrGreen, arrBlue) = U.unzip3 arrRGB
	let comps                       = [arrRed, arrGreen, arrBlue]
	
	(comps', tElapsed)
	 <- time $ let	comps' 	= P.map (process iterations) comps
		   in	comps' `deepSeqArrays` return comps'
	
	putStr $ prettyTime tElapsed

        let [arrRed', arrGreen', arrBlue'] = comps'
	writeImageToBMP fileOut
	        (U.zip3 arrRed' arrGreen' arrBlue')


{-# NOINLINE process #-}
process	:: Int -> Array U DIM2 Word8 -> Array U DIM2 Word8
process iterations = demote . blur iterations . promote

	
{-# NOINLINE promote #-}
promote	:: Array U DIM2 Word8 -> Array U DIM2 Double
promote arr
 = arr `deepSeqArray` compute
 $ A.map ffs arr

 where	{-# INLINE ffs #-}
	ffs	:: Word8 -> Double
	ffs x	=  fromIntegral (fromIntegral x :: Int)


{-# NOINLINE demote #-}
demote	:: Array U DIM2 Double -> Array U DIM2 Word8
demote arr
 = arr `deepSeqArray` compute
 $ A.map ffs arr

 where	{-# INLINE ffs #-}
	ffs 	:: Double -> Word8
	ffs x	=  fromIntegral (truncate x :: Int)


{-# NOINLINE blur #-}
blur 	:: Int -> Array U DIM2 Double -> Array U DIM2 Double
blur !iterations arrInit
 = go iterations arrInit
 where  go :: Int -> Array U DIM2 Double -> Array U DIM2 Double
        go !0 !arr = arr
        go !n !arr  
 	 = arr `deepSeqArray` go (n-1) 
 	 $ compute
	 $ A.map (/ 159)
	 $ forStencil2 BoundClamp arr
	   [stencil2|	2  4  5  4  2
			4  9 12  9  4
			5 12 15 12  5
			4  9 12  9  4
			2  4  5  4  2 |]
			

{- version using convolveOut
-- | Run several iterations of blurring.
blur 	:: Int -> Array DIM2 Double -> Array DIM2 Double
blur 0 arr	= arr
blur n arr	= blurs (n - 1) (force $ blur arr)

-- | Run a single iteration of blurring.
blur :: Array DIM2 Double -> Array DIM2 Double
blur input@Manifest{}
 = convolveOut outClamp kernel input
 where kernel 	= force 
		$ A.fromList (Z :. 5 :. 5) 
		$ Data.List.map (\x -> x / 159) 
			 [2.0,  4.0,  5.0,  4.0, 2.0,
                          4.0,  9.0, 12.0,  9.0, 4.0,
                          5.0, 12.0, 15.0, 12.0, 5.0,
                          4.0,  9.0, 12.0,  9.0, 4.0,
                          2.0,  4.0,  5.0,  4.0, 2.0]
-}	
	