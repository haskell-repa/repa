{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

-- TODO: This needs -funfolding-creation-threshold1000

import Data.List
import Control.Monad
import System.Environment
import Data.Word
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing
import Data.Array.Repa.Algorithms.Iterate
import Data.Array.Repa 			as A
import Data.Array.Repa.Stencil		as A
import Prelude				as P

main 
 = do	args	<- getArgs
	case args of
	 [iterations, fileIn, fileOut]	-> run (read iterations) fileIn fileOut
	 _				-> usage

usage 	= putStr $ unlines
	[ "repa-blur <iterations> <fileIn.bmp> <fileOut.bmp>" ]
	
run iterations fileIn fileOut
 = do	comps	<- liftM (either (error . show) id) 
		$  readComponentsListFromBMP fileIn

	comps `deepSeqArrays` return ()
	
	(comps', tElapsed)
	 <- time $ let	comps' 	= P.map (process iterations) comps
		   in	comps' `deepSeqArrays` return comps'
	
	putStr $ prettyTime tElapsed
			
	writeComponentsListToBMP fileOut comps'

{-# NOINLINE process #-}
process	:: Int -> Array DIM2 Word8 -> Array DIM2 Word8
process iterations = demote . blur iterations . promote

	
{-# NOINLINE promote #-}
promote	:: Array DIM2 Word8 -> Array DIM2 Double
promote arr@(Array _ [Region RangeAll (GenManifest _)])
 = arr `deepSeqArray` force
 $ A.map ffs arr

 where	{-# INLINE ffs #-}
	ffs	:: Word8 -> Double
	ffs x	=  fromIntegral (fromIntegral x :: Int)


{-# NOINLINE demote #-}
demote	:: Array DIM2 Double -> Array DIM2 Word8
demote arr@(Array _ [Region RangeAll (GenManifest _)])
 = arr `deepSeqArray` force
 $ A.map ffs arr

 where	{-# INLINE ffs #-}
	ffs 	:: Double -> Word8
	ffs x	=  fromIntegral (truncate x :: Int)


{-# NOINLINE blur #-}
blur 	:: Int -> Array DIM2 Double -> Array DIM2 Double
blur !iterations arr@(Array _ [Region RangeAll (GenManifest _)])
 	= arr `deepSeqArray` force2
	$ iterateBlockwise' iterations arr
	$ A.map (/ 159)
	. mapStencil2 BoundClamp
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
	