{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

-- | Canny edge detector
import Data.List
import Data.Word
import Control.Monad
import System.Environment
import Data.Array.Repa 			as R
import Data.Array.Repa.Stencil
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing
import Prelude				hiding (compare)

type Image	= Array DIM2 Double

-- Constants ------------------------------------------------------------------
orientPosDiag	= 0	:: Double
orientVert	= 1	:: Double
orientNegDiag	= 2	:: Double
orientHoriz	= 3	:: Double
orientUndef	= 4	:: Double

edge False	= 0 	:: Double
edge True	= 200 	:: Double


-- Main routine ---------------------------------------------------------------
main 
 = do	args	<- getArgs
	case args of
	 [fileIn, fileOut]	-> run fileIn fileOut
	 _			-> putStr "repa-edgedetect <fileIn.bmp> <fileOut.bmp>"


run fileIn fileOut
 = do	arrInput 	<- liftM (force . either (error . show) id) 
			$ readImageFromBMP fileIn
	
	arrInput `deepSeqArray` return ()
	arrGrey		<- timeStage "toGreyScale" $ \_ -> toGreyScale    arrInput
	arrBlured	<- timeStage "blur" 	   $ \_ -> blur           arrGrey
	arrDX		<- timeStage "diffX"	   $ \_ -> gradientX      arrBlured
	arrDY		<- timeStage "diffY"	   $ \_ -> gradientY      arrBlured
	arrMag		<- timeStage "magnitude"   $ \_ -> gradientMag    arrDX arrDY
	arrOrient	<- timeStage "orientation" $ \_ -> gradientOrient arrDX arrDY
	arrSupress	<- timeStage "suppress"    $ \_ -> suppress       arrMag arrOrient
	writeMatrixToGreyscaleBMP fileOut arrSupress


timeStage
	:: (Shape sh, Elt a)
	=> String 
	-> (() -> Array sh a)
	-> IO (Array sh a)

{-# NOINLINE timeStage #-}
timeStage name fn
 = do	(arrResult, t)
		<- time $ let arrResult' = fn ()
			  in  arrResult' `deepSeqArray` return arrResult'

	putStr 	$  name ++ "\n"
		++ unlines [ "  " ++ l | l <- lines $ prettyTime t ]

	return arrResult
	

-------------------------------------------------------------------------------

-- | RGB to greyscale conversion.
{-# NOINLINE toGreyScale #-}
toGreyScale :: Array DIM3 Word8 -> Array DIM2 Double
toGreyScale 
	arr@(Array _ [Region RangeAll (GenManifest _)])
  = arr `deepSeqArray` force
  $ traverse arr
	(\(sh :. _) -> sh)
	(\get ix    -> rgbToLuminance 
				(get (ix :. 0))
				(get (ix :. 1))
				(get (ix :. 2)))

 where	{-# INLINE rgbToLuminance #-}
	rgbToLuminance :: Word8 -> Word8 -> Word8 -> Double
	rgbToLuminance r g b 
		= fromIntegral r * 0.3
		+ fromIntegral g * 0.59
		+ fromIntegral b * 0.11


-- | Perform a Gaussian blur to the image.
{-# NOINLINE blur #-}
blur 	:: Array DIM2 Double -> Array DIM2 Double
blur 	arr@(Array _ [Region RangeAll (GenManifest _)])	
	= arr `deepSeqArray` force2
	$ R.map (/ 159) 
	$ forStencil2  BoundClamp arr
	  [stencil2|	2  4  5  4  2
			4  9 12  9  4
			5 12 15 12  5
			4  9 12  9  4
			2  4  5  4  2 |]


-- | Compute gradient in the X direction.
{-# NOINLINE gradientX #-}
gradientX :: Image -> Image
gradientX img@(Array _ [Region RangeAll (GenManifest _)])
 	= img `deepSeqArray` 
    	  force2 $ forStencil2 BoundClamp img
	  [stencil2|	-1  0  1
			-2  0  2
			-1  0  1 |]


-- | Compute gradient in the Y direction.
{-# NOINLINE gradientY #-}
gradientY :: Image -> Image
gradientY img@(Array _ [Region RangeAll (GenManifest _)])
	= img `deepSeqArray` 
	  force2 $ forStencil2 BoundClamp img
	  [stencil2|	 1  2  1
			 0  0  0
			-1 -2 -1 |] 


-- | Compute magnitude of the vector gradient.
{-# NOINLINE gradientMag #-}
gradientMag :: Image -> Image -> Image
gradientMag
	dX@(Array _ [Region RangeAll (GenManifest _)])
	dY@(Array _ [Region RangeAll (GenManifest _)])
 = [dX, dY] `deepSeqArrays`
   force2 $ R.zipWith magnitude  dX dY

 where	{-# INLINE magnitude #-}
	magnitude :: Double -> Double -> Double
	magnitude x y	= x * x + y * y


-- | Classify the orientation of the vector gradient.
{-# NOINLINE gradientOrient #-}
gradientOrient :: Image -> Image -> Image
gradientOrient
 	dX@(Array _ [Region RangeAll (GenManifest _)])
	dY@(Array _ [Region RangeAll (GenManifest _)])
 = [dX, dY] `deepSeqArrays`
   force2 $ R.zipWith orientation dX dY

 where	{-# INLINE orientation #-}
	orientation :: Double -> Double -> Double
	orientation x y
 	 | x >= -40, x < 40
 	 , y >= -40, y < 40	= orientUndef

	 | otherwise
	 = let	-- determine the angle of the vector and rotate it around a bit
		-- to make the segments easier to classify.
		!d	= atan2 y x 
		!dRot	= (d - (pi/8)) * (4/pi)
	
		-- normalise angle to beween 0..8
		!dNorm	= if dRot < 0 then dRot + 8 else dRot

		-- doing tests seems to be faster than using floor.
	   in	if dNorm >= 4
		 then if dNorm >= 6
			then if dNorm >= 7
				then orientHoriz   -- 7
				else orientNegDiag -- 6

			else if dNorm >= 5
				then orientVert    -- 5
				else orientPosDiag -- 4

		 else if dNorm >= 2
			then if dNorm >= 3
				then orientHoriz   -- 3
				else orientNegDiag -- 2

			else if dNorm >= 1
				then orientVert    -- 1
				else orientPosDiag -- 0


-- | Suppress pixels which are not local maxima.
{-# NOINLINE suppress #-}
suppress :: Image -> Image -> Image
suppress   dMag@(Array _ [Region RangeAll (GenManifest _)]) 
	dOrient@(Array _ [Region RangeAll (GenManifest _)])
 = [dMag, dOrient] `deepSeqArrays` force
 $ traverse2 dMag dOrient const compare
 where
	_ :. height :. width	= extent dMag
	
	{-# INLINE isBoundary #-}
	isBoundary i j 
         | i == 0 || j == 0     = True
	 | i == width  - 1	= True
	 | j == height - 1	= True
	 | otherwise            = False

	{-# INLINE compare #-}
	compare get1 get2 d@(sh :. i :. j)
         | isBoundary i j      = edge False 
         | o == orientHoriz    = isMaximum (get1 (sh :. i     :. j - 1)) (get1 (sh :. i     :. j + 1)) 
         | o == orientVert     = isMaximum (get1 (sh :. i - 1 :. j))     (get1 (sh :. i + 1 :. j)) 
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
