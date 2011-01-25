{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

import Data.List
import Control.Monad
import System.Environment
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
	
-- TODO: component-wise is filthy.
--       do this with a DIM3 array.
run iterations fileIn fileOut
 = do	(arrRed, arrGreen, arrBlue)
		<- liftM (either (error . show) id) 
		$  readComponentsFromBMP fileIn

	arrRed `deepSeqArray` arrGreen `deepSeqArray` arrBlue `deepSeqArray` return ()

	((arrRed', arrGreen', arrBlue'), tElapsed)
		<- time $ let result	= blurComponents iterations arrRed arrGreen arrBlue
			  in  result `seq` return result
	
	putStr $ prettyTime tElapsed
			
	writeComponentsToBMP fileOut arrRed' arrGreen' arrBlue'



-- | Blur all the components of an image.
blurComponents iterations arrRed arrGreen arrBlue
 = let	process arr	
		= force 
		$ A.map truncate 
		$ blurs iterations
		$ force
		$ A.map fromIntegral 
		$ arr

	[arrRed', arrGreen', arrBlue']
		= P.map process [arrRed, arrGreen, arrBlue]
 
   in	arrRed' `deepSeqArray` arrGreen' `deepSeqArray` arrBlue' `deepSeqArray`
         (arrRed', arrGreen', arrBlue')


blurs 	:: Int -> Array DIM2 Double -> Array DIM2 Double
blurs steps arr
 	= iterateBlockwise' steps arr
	$ A.map (/ 159)
	. mapStencil2 BoundClamp
	  [stencil2|	2  4  5  4  2
			4  9 12  9  4
			5 12 15 12  5
			4  9 12  9  4
			2  4  5  4  2 |]
			
{-
-- | Run several iterations of blurring.
blurs 	:: Int -> Array DIM2 Double -> Array DIM2 Double
blurs 0 arr	= arr
blurs n arr	= blurs (n - 1) (force $ blur arr)


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
	