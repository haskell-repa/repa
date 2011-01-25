-- | Canny edge detector

{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

import Data.List
import Data.Word
import Control.Monad
import System.Environment
import Data.Array.Repa 			as Repa
import Data.Array.Repa.Stencil
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing
import Prelude				hiding (compare)

-- Constants ------------------------------------------------------------------
orientUndef	= 0	:: Double
orientHoriz	= 1	:: Double
orientVert	= 2	:: Double
orientPosDiag	= 3	:: Double
orientNegDiag	= 4	:: Double

edge False	= 0 	:: Double
edge True	= 200 	:: Double


-- Main routine ---------------------------------------------------------------
main 
 = do	args	<- getArgs
	case args of
	 [fileIn, fileOut]	-> run fileIn fileOut
	 _			-> usage


run fileIn fileOut
 = do	inputImage 	<- liftM (force . either (error . show) id) 
			$ readImageFromBMP fileIn
	
	inputImage `deepSeqArray` return ()
	
	(arrResult, tElapsed)
		<- time $ let result	= floatToRgb $ canny inputImage
			  in  result `deepSeqArray` return result
	
	putStr $ prettyTime tElapsed

	writeImageToBMP fileOut arrResult

usage
 = putStr $ unlines
	[ "repa-edgedetect <fileIn.bmp> <fileOut.bmp>" ]


-- Edge detection -------------------------------------------------------------
canny 	:: Array DIM3 Word8 
	-> Array DIM2 Double

{-# NOINLINE canny #-}
canny input@Manifest{}
 = force output
    where
      blured 	= blur $ toGreyScale input
      dX 	= gradientXCompute blured
      dY 	= gradientYCompute blured
      mag 	= gradientIntensityCompute   dX dY
      orient 	= gradientOrientationCompute dX dY
      output 	= nonMaximumSupression (force mag) (force orient)


-- | Maximum suppression	
nonMaximumSupression 
	:: Array DIM2 Double
	-> Array DIM2 Double
	-> Array DIM2 Double

{-# INLINE nonMaximumSupression #-}
nonMaximumSupression dMag@Manifest{} dOrient@Manifest{}
 = dMag `deepSeqArray` dOrient `deepSeqArray` force
 $ traverse2 dMag dOrient const compare
 where
	{-# INLINE isBoundary #-}
	isBoundary i j 
         | i == 0 || j == 0     = True
	 | i == width  dMag - 1 = True
	 | j == height dMag - 1 = True
	 | otherwise            = False

	{-# INLINE compare #-}
	compare get1 get2 d@(sh :. i :. j)
         | isBoundary i j      = edge False 
         | o == orientHoriz    = isMaximum (get1 (sh :. i - 1 :. j))     (get1 (sh :. i + 1 :. j)) 
         | o == orientVert     = isMaximum (get1 (sh :. i     :. j - 1)) (get1 (sh :. i     :. j + 1)) 
         | o == orientPosDiag  = isMaximum (get1 (sh :. i - 1 :. j - 1)) (get1 (sh :. i + 1 :. j + 1)) 
         | o == orientNegDiag  = isMaximum (get1 (sh :. i - 1 :. j + 1)) (get1 (sh :. i + 1 :. j - 1)) 
         | otherwise           = edge False  
      
         where
          !o 		= get2 d  
          !intensity 	= get1 (Z :. i :. j)

	  {-# INLINE isMaximum #-}
          isMaximum intensity1 intensity2
            | intensity < intensity1 = edge False
            | intensity < intensity2 = edge False
            | otherwise              = edge True


-- XY Gradient calculation
gradientXCompute :: Array DIM2 Double -> Array DIM2 Double
gradientXCompute arr@Manifest{}
	= arr `deepSeqArray` forceBlockwise
	$ forStencil2 BoundClamp arr
	  [stencil2|	-1  0  1
			-2  0  2
			-1  0  1 |]

gradientYCompute :: Array DIM2 Double -> Array DIM2 Double
gradientYCompute arr@Manifest{}
	= arr `deepSeqArray` forceBlockwise 
	$ forStencil2 BoundClamp arr
	  [stencil2|	 1  2  1
			 0  0  0
			-1 -2 -1 |] 


gradientIntensityCompute :: Array DIM2 Double -> Array DIM2 Double -> Array DIM2 Double
{-# INLINE gradientIntensityCompute #-}
gradientIntensityCompute dX@Manifest{} dY@Manifest{}
	= dX `deepSeqArray` dY `deepSeqArray` force
	$ Repa.zipWith (\x y -> sqrt(x*x + y*y)) dX dY


gradientOrientationCompute :: Array DIM2 Double -> Array DIM2 Double -> Array DIM2 Double
{-# INLINE gradientOrientationCompute #-}
gradientOrientationCompute dX@Manifest{} dY@Manifest{}
	= dX `deepSeqArray` dY `deepSeqArray` force
	$ Repa.zipWith orientation dX dY


orientation :: Double -> Double -> Double
{-# INLINE orientation #-}
orientation x y 
	| x >= -40, x < 40, y > -40, y < 40	= orientUndef
	| d >= (-7 * pi8), d < (-5 * pi8)	= orientPosDiag
	| d >= (-5 * pi8), d < (-3 * pi8)	= orientVert
	| d >= (-3 * pi8), d < (-1 * pi8)	= orientNegDiag
	| d >= (-1 * pi8), d < ( 1 * pi8)	= orientHoriz
	| d >= ( 1 * pi8), d < ( 3 * pi8)	= orientPosDiag
	| d >= ( 3 * pi8), d < ( 5 * pi8)	= orientVert
	| d >= ( 5 * pi8), d < ( 7 * pi8)	= orientNegDiag
	| otherwise				= orientHoriz
	
	where	!d	= atan2 y x
		!pi8	= pi / 8


{-# NOINLINE blur #-}
blur 	:: Array DIM2 Double -> Array DIM2 Double
blur arr@Manifest{}	
	= arr `deepSeqArray` Repa.forceBlockwise
	$ Repa.map (/ 159)
	$ forStencil2  BoundClamp arr
	  [stencil2|	2  4  5  4  2
			4  9 12  9  4
			5 12 15 12  5
			4  9 12  9  4
			2  4  5  4  2 |]


-- | RGB to greyscale conversion.
toGreyScale
        :: Array DIM3 Word8
        -> Array DIM2 Double
        
{-# NOINLINE toGreyScale #-}
toGreyScale arr@Manifest{}
  = arr `deepSeqArray` force 
  $ traverse arr
	(\(sh :. _) -> sh)
	(\get ix    -> rgbToLuminance 
				(get (ix :. 0))
				(get (ix :. 1))
				(get (ix :. 2)))


-- | Convert a RGB value to a luminance.
rgbToLuminance :: Word8 -> Word8 -> Word8 -> Double
{-# INLINE rgbToLuminance #-}
rgbToLuminance r g b 
	= fromIntegral r * 0.3
	+ fromIntegral g * 0.59
	+ fromIntegral b * 0.11


-- | Convert a float array to greyscale components.
floatToRgb :: Array DIM2 Double -> Array DIM3 Word8
floatToRgb arrDoubles@Manifest{}
 = force $ traverse arrDoubles
        (\sh -> sh :. 4)                
        (\get (ix :. c) 
	  -> case c of
     		0	-> truncate (get ix)
		1	-> truncate (get ix)
		2	-> truncate (get ix)
		3	-> 0)
