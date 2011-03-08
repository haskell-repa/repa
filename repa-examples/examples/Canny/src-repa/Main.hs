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

type Image	= Array DIM2 Float

-- Constants ------------------------------------------------------------------
orientPosDiag	= 0	:: Float
orientVert	= 1	:: Float
orientNegDiag	= 2	:: Float
orientHoriz	= 3	:: Float
-- orientUndef	= 4	:: Float


data Edge	= None | Weak | Strong
edge None	= 0 	:: Float
edge Weak	= 100 	:: Float
edge Strong	= 200	:: Float


-- Main routine ---------------------------------------------------------------
main 
 = do	args	<- getArgs
	case args of
	 [fileIn, fileOut]		
	   -> run 0 50 100 fileIn fileOut

	 [loops, threshLow, threshHigh, fileIn, fileOut]
	   -> run (read loops) (read threshLow) (read threshHigh) fileIn fileOut

	 _ -> putStrLn "repa-canny [loops threshLow threshHigh] <fileIn.bmp> <fileOut.bmp>"


run loops threshLow threshHigh fileIn fileOut
 = do	arrInput 	<- liftM (force . either (error . show) id) 
			$ readImageFromBMP fileIn

	arrInput `deepSeqArray` return ()
	(arrResult, tTotal)
	 <- time
	 $ do	arrGrey		<- timeStage loops "toGreyScale"  $ return $ toGreyScale    arrInput
		arrBluredX	<- timeStage loops "blurX" 	  $ return $ blurSepX       arrGrey
		arrBlured	<- timeStage loops "blurY" 	  $ return $ blurSepY       arrBluredX
		arrDX		<- timeStage loops "diffX"	  $ return $ gradientX      arrBlured
		arrDY		<- timeStage loops "diffY"	  $ return $ gradientY      arrBlured
		arrMag		<- timeStage loops "magnitude"    $ return $ gradientMag    arrDX arrDY

		arrOrient	<- timeStage loops "orientation"  
				$ return $ gradientOrient threshLow arrDX arrDY

		arrSupress	<- timeStage loops "suppress"     
				$ return $ suppress threshLow threshHigh arrMag arrOrient

		return arrSupress

	when (loops >= 1)
	 $ putStrLn $ "\nTOTAL\n"
	
	putStr $ prettyTime tTotal
	
	writeMatrixToGreyscaleBMP fileOut arrResult


-- | Wrapper to time each stage of the algorithm.
timeStage
	:: (Shape sh, Elt a)
	=> Int
	-> String 
	-> (IO (Array sh a))
	-> (IO (Array sh a))

{-# NOINLINE timeStage #-}
timeStage loops name fn
 = do	let burn !n
	     = do arr	<- fn
		  arr `deepSeqArray` return ()
		  if n <= 1 then return arr
		            else burn (n - 1)
			
	(arrResult, t)
	 <- time $ do	arrResult' <- burn loops
		   	arrResult' `deepSeqArray` return arrResult'

	when (loops >= 1) 
	 $ putStr 	$  name ++ "\n"
			++ unlines [ "  " ++ l | l <- lines $ prettyTime t ]

	return arrResult
	

-------------------------------------------------------------------------------
-- | RGB to greyscale conversion.
{-# NOINLINE toGreyScale #-}
toGreyScale :: Array DIM3 Word8 -> Array DIM2 Float
toGreyScale 
	arr@(Array _ [Region RangeAll (GenManifest _)])
  = arr `deepSeqArray` force2
  $ traverse arr
	(\(sh :. _) -> sh)
	(\get ix    -> rgbToLuminance 
				(get (ix :. 0))
				(get (ix :. 1))
				(get (ix :. 2)))

 where	{-# INLINE rgbToLuminance #-}
	rgbToLuminance :: Word8 -> Word8 -> Word8 -> Float
	rgbToLuminance r g b 
		= floatOfWord8 r * 0.3
		+ floatOfWord8 g * 0.59
		+ floatOfWord8 b * 0.11

	{-# INLINE floatOfWord8 #-}
	floatOfWord8 :: Word8 -> Float
	floatOfWord8 w8
	 	= fromIntegral (fromIntegral w8 :: Int)


-- | Separable Gaussian blur in the X direction.
{-# NOINLINE blurSepX #-}
blurSepX :: Array DIM2 Float -> Array DIM2 Float
blurSepX arr@(Array _ [Region RangeAll (GenManifest _)])	
	= arr `deepSeqArray` force2
	$ forStencil2  BoundClamp arr
	  [stencil2|	1 4 6 4 1 |]	

-- | Separable Gaussian blur in the Y direction.
{-# NOINLINE blurSepY #-}
blurSepY :: Array DIM2 Float -> Array DIM2 Float
blurSepY arr@(Array _ [Region RangeAll (GenManifest _)])	
	= arr `deepSeqArray` force2
	$ R.map (/ 256)
	$ forStencil2  BoundClamp arr
	  [stencil2|	1
	 		4
			6
			4
			1 |]	


-- | Compute gradient in the X direction.
{-# NOINLINE gradientX #-}
gradientX :: Image -> Image
gradientX img@(Array _ [Region RangeAll (GenManifest _)])
 	= img `deepSeqArray` force2
    	$ forStencil2 BoundClamp img
	  [stencil2|	-1  0  1
			-2  0  2
			-1  0  1 |]


-- | Compute gradient in the Y direction.
{-# NOINLINE gradientY #-}
gradientY :: Image -> Image
gradientY img@(Array _ [Region RangeAll (GenManifest _)])
	= img `deepSeqArray` force2
	$ forStencil2 BoundClamp img
	  [stencil2|	 1  2  1
			 0  0  0
			-1 -2 -1 |] 


-- | Compute magnitude of the vector gradient.
{-# NOINLINE gradientMag #-}
gradientMag :: Image -> Image -> Image
gradientMag
	dX@(Array _ [Region RangeAll (GenManifest _)])
	dY@(Array _ [Region RangeAll (GenManifest _)])
 = [dX, dY] `deepSeqArrays` force2
 $ R.zipWith magnitude  dX dY

 where	{-# INLINE magnitude #-}
	magnitude :: Float -> Float -> Float
	magnitude x y	= sqrt (x * x + y * y)


-- | Classify the orientation of the vector gradient.
{-# NOINLINE gradientOrient #-}
gradientOrient :: Float -> Image -> Image -> Image
gradientOrient !threshLow 
 	dX@(Array _ [Region RangeAll (GenManifest _)])
	dY@(Array _ [Region RangeAll (GenManifest _)])
 = [dX, dY] `deepSeqArrays` force2
 $ R.zipWith orientation dX dY

 where	{-# INLINE orientation #-}
	orientation :: Float -> Float -> Float
	orientation x y

	 -- Don't bother computing orientation if vector is below threshold.
{- 	 | x >= negate threshLow, x < threshLow
 	 , y >= negate threshLow, y < threshLow
 	 = orientUndef
-}
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
suppress :: Float -> Float -> Image -> Image -> Image
suppress threshLow threshHigh
	   dMag@(Array _ [Region RangeAll (GenManifest _)]) 
	dOrient@(Array _ [Region RangeAll (GenManifest _)])
 = [dMag, dOrient] `deepSeqArrays` force2 
 $ traverse2 dMag dOrient const compare

 where	_ :. height :. width	= extent dMag
	
	{-# INLINE isBoundary #-}
	isBoundary i j 
         | i == 0 || j == 0     = True
	 | i == width  - 1	= True
	 | j == height - 1	= True
	 | otherwise            = False

	{-# INLINE compare #-}
	compare getMag getOrient d@(sh :. i :. j)
         | isBoundary i j	= edge None
         | o == orientHoriz	= isMaximum (getMag (sh :. i   :. j-1)) (getMag (sh :. i   :. j+1)) 
         | o == orientVert	= isMaximum (getMag (sh :. i-1 :. j))   (getMag (sh :. i+1 :. j)) 
         | o == orientNegDiag	= isMaximum (getMag (sh :. i-1 :. j-1)) (getMag (sh :. i+1 :. j+1)) 
         | o == orientPosDiag	= isMaximum (getMag (sh :. i-1 :. j+1)) (getMag (sh :. i+1 :. j-1)) 
         | otherwise 		= edge None
      
         where
          !o 		= getOrient d  
          !m		= getMag    (Z :. i :. j)

	  {-# INLINE isMaximum #-}
          isMaximum intensity1 intensity2
            | m < intensity1 	= edge None
            | m < intensity2 	= edge None
            | m < threshLow 	= edge None
	    | m < threshHigh	= edge Weak
	    | otherwise		= edge Strong

