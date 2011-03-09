{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

-- | Canny edge detector
import Data.List
import Data.Word
import Data.Bits
import Data.Int
import Control.Monad
import System.Environment
import Data.Array.Repa 			as R
import Data.Array.Repa.Stencil
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing
import Prelude				hiding (compare)

import System.IO.Unsafe
import qualified Data.Vector.Unboxed.Mutable	as VM
import qualified Data.Vector.Unboxed		as V

type Image a	= Array DIM2 a

-- Constants ------------------------------------------------------------------
-- TODO: Setting these to Word8 triggers a bug in the LLVM Mangler
orientPosDiag	= 0	:: Float
orientVert	= 1	:: Float
orientNegDiag	= 2	:: Float
orientHoriz	= 3	:: Float
orientUndef	= 4	:: Float


data Edge	= None | Weak | Strong
edge None	= 0 	:: Word8
edge Weak	= 128 	:: Word8
edge Strong	= 255	:: Word8


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
		
		arrMagOrient	<- timeStage loops "magOrient"	 
				$ return $ gradientMagOrient threshLow arrDX arrDY

		arrSuppress	<- timeStage loops "suppress"     
				$ return $ suppress threshLow threshHigh arrMagOrient

		arrStrong	<- timeStage loops "select"	$ return $ selectStrong arrSuppress	
		arrEdges	<- timeStage loops "wildfire"	$ return $ wildfire arrSuppress arrStrong	

		return arrEdges

	when (loops >= 1)
	 $ putStrLn $ "\nTOTAL\n"
	
	putStr $ prettyTime tTotal
	
	writeComponentsToBMP fileOut arrResult arrResult arrResult


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


{-# INLINE floatOfWord8 #-}
floatOfWord8 :: Word8 -> Float
floatOfWord8 w8
 	= fromIntegral (fromIntegral w8 :: Int)


{-# INLINE word8OfFloat #-}
word8OfFloat :: Float -> Word8
word8OfFloat f
 	= fromIntegral (truncate f :: Int)


{-# INLINE int8OfFloat #-}
int8OfFloat :: Float -> Int8
int8OfFloat f
 	= fromIntegral (truncate f :: Int)
	

-------------------------------------------------------------------------------
-- | RGB to greyscale conversion.
{-# NOINLINE toGreyScale #-}
toGreyScale :: Array DIM3 Word8 -> Array DIM2 Word8
toGreyScale 
	arr@(Array _ [Region RangeAll (GenManifest _)])
  = arr `deepSeqArray` force2
  $ unsafeTraverse arr
	(\(sh :. _) -> sh)
	(\get ix    -> rgbToLuminance 
				(get (ix :. 0))
				(get (ix :. 1))
				(get (ix :. 2)))

 where	{-# INLINE rgbToLuminance #-}
	rgbToLuminance :: Word8 -> Word8 -> Word8 -> Word8
	rgbToLuminance r g b 
	 = word8OfFloat 
		( floatOfWord8 r * 0.3
		+ floatOfWord8 g * 0.59
		+ floatOfWord8 b * 0.11)



-- | Separable Gaussian blur in the X direction.
{-# NOINLINE blurSepX #-}
blurSepX :: Array DIM2 Word8 -> Array DIM2 Float
blurSepX arr@(Array _ [Region RangeAll (GenManifest _)])	
	= arr `deepSeqArray` force2
	$ forStencil2  BoundClamp arr floatOfWord8
	  [stencil2|	1 4 6 4 1 |]	


-- | Separable Gaussian blur in the Y direction.
{-# NOINLINE blurSepY #-}
blurSepY :: Array DIM2 Float -> Array DIM2 Float
blurSepY arr@(Array _ [Region RangeAll (GenManifest _)])	
	= arr `deepSeqArray` force2
	$ R.map (/ 256)
	$ forStencil2  BoundClamp arr id
	  [stencil2|	1
	 		4
			6
			4
			1 |]


-- | Compute gradient in the X direction.
{-# NOINLINE gradientX #-}
gradientX :: Array DIM2 Float -> Array DIM2 Float
gradientX img@(Array _ [Region RangeAll (GenManifest _)])
 	= img `deepSeqArray` force2
    	$ forStencil2 (BoundConst 0) img id
	  [stencil2|	-1  0  1
			-2  0  2
			-1  0  1 |]


-- | Compute gradient in the Y direction.
{-# NOINLINE gradientY #-}
gradientY :: Array DIM2 Float -> Array DIM2 Float
gradientY img@(Array _ [Region RangeAll (GenManifest _)])
	= img `deepSeqArray` force2
	$ forStencil2 (BoundConst 0) img id
	  [stencil2|	 1  2  1
			 0  0  0
			-1 -2 -1 |] 


-- | Classify the orientation of the vector gradient.
{-# NOINLINE gradientMagOrient #-}
gradientMagOrient :: Float -> Image Float -> Image Float -> Image (Float, Float)
gradientMagOrient !threshLow 
 	dX@(Array _ [Region RangeAll (GenManifest _)])
	dY@(Array _ [Region RangeAll (GenManifest _)])

 = [dX, dY] `deepSeqArrays` force2
 $ R.zipWith magOrient dX dY

 where	{-# INLINE magOrient #-}
	magOrient :: Float -> Float -> (Float, Float)
	magOrient x y
		= (magnitude x y, orientation x y)
	
	{-# INLINE magnitude #-}
	magnitude :: Float -> Float -> Float
	magnitude x y
		= sqrt (x * x + y * y)
	
	{-# INLINE orientation #-}
	orientation :: Float -> Float -> Float
	orientation x y

	 -- Don't bother computing orientation if vector is below threshold.
 	 | x >= negate threshLow, x < threshLow
 	 , y >= negate threshLow, y < threshLow
 	 = orientUndef

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
suppress :: Float -> Float -> Image (Float, Float) -> Image Word8
suppress threshLow threshHigh
	   dMagOrient@(Array _ [Region RangeAll (GenManifest _)]) 
 = dMagOrient `deepSeqArray` force2 
 $ unsafeTraverse dMagOrient id compare

 where	_ :. height :. width	= extent dMagOrient
	
	{-# INLINE isBoundary #-}
	isBoundary i j 
         | i == 0 || j == 0     = True
	 | i == width  - 1	= True
	 | j == height - 1	= True
	 | otherwise            = False

	{-# INLINE compare #-}
	compare getMagOrient d@(sh :. i :. j)
         | isBoundary i j	= edge None
         | o == orientHoriz	= isMaximum (getMag (sh :. i   :. j-1)) (getMag (sh :. i   :. j+1)) 
         | o == orientVert	= isMaximum (getMag (sh :. i-1 :. j))   (getMag (sh :. i+1 :. j)) 
         | o == orientNegDiag	= isMaximum (getMag (sh :. i-1 :. j-1)) (getMag (sh :. i+1 :. j+1)) 
         | o == orientPosDiag	= isMaximum (getMag (sh :. i-1 :. j+1)) (getMag (sh :. i+1 :. j-1)) 
         | otherwise 		= edge None
      
         where
          !o 		= getOrient d  
          !m		= getMag    (Z :. i :. j)
	
	  getMag 	= fst . getMagOrient
	  getOrient	= snd . getMagOrient

	  {-# INLINE isMaximum #-}
          isMaximum intensity1 intensity2
            | m < intensity1 	= edge None
            | m < intensity2 	= edge None
            | m < threshLow 	= edge None
	    | m < threshHigh	= edge Weak
	    | otherwise		= edge Strong


-- | Select indices of strong edges
--   TODO: merge this into suppress above with a fused map/select operation.
{-# NOINLINE selectStrong #-}
selectStrong :: Image Word8 -> Array DIM1 Int
selectStrong img@(Array _ [Region RangeAll (GenManifest _)])
 = img `deepSeqArray` 
   let 	{-# INLINE match #-}
	match ix	= img R.! ix == edge Strong

	{-# INLINE process #-}
	process ix	= toIndex (extent img) ix
	
   in	select match process (extent img)


-- | Burn in any weak edges that are connected to strong edges.
{-# NOINLINE wildfire #-}
wildfire 
	:: Image Word8		-- ^ Image with strong and weak edges set.
	-> Array DIM1 Int	-- ^ Array containing flat indices of strong edges.
	-> Image Word8

wildfire img@(Array _ [Region RangeAll (GenManifest _)])
         arrStrong
 = unsafePerformIO 
 $ do	(sh, vec)	<- wildfireIO 
	return	$ sh `seq` vec `seq` 
		  Array sh [Region RangeAll (GenManifest vec)]

 where	lenImg		= R.size $ R.extent img
	lenStrong	= R.size $ R.extent arrStrong
	vStrong		= toVector arrStrong
	
	wildfireIO
  	 = do	-- Stack of image indices we still need to consider.
		vStrong' <- V.thaw vStrong
		vStack	 <- VM.grow vStrong' (lenImg - lenStrong)
	
		-- Burn in new edges.
		vImg	<- VM.unsafeNew lenImg
		VM.set vImg 0
		burn vImg vStack lenStrong
		vImg'	<- V.unsafeFreeze vImg
		return	(extent img, vImg')

	
	burn :: VM.IOVector Word8 -> VM.IOVector Int -> Int -> IO ()
	burn !vImg !vStack !top
	 | top == 0
	 = return ()
	
	 | otherwise
	 = do	let !top'		=  top - 1
		n			<- VM.unsafeRead vStack top'
		let (Z :. y :. x)	= fromIndex (R.extent img) n

		let {-# INLINE push #-}
		    push t		= pushWeak vImg vStack t
				
		VM.write vImg n (edge Strong)
	    	 >>  push (Z :. y - 1 :. x - 1) top'
	    	 >>= push (Z :. y - 1 :. x    )
	    	 >>= push (Z :. y - 1 :. x + 1)

	    	 >>= push (Z :. y     :. x - 1)
	    	 >>= push (Z :. y     :. x + 1)

	    	 >>= push (Z :. y + 1 :. x - 1)
	    	 >>= push (Z :. y + 1 :. x    )
	    	 >>= push (Z :. y + 1 :. x + 1)

	    	 >>= burn vImg vStack

	-- If this ix is weak in the source then set it to strong in the
	-- result and push the ix onto the stack.
	{-# INLINE pushWeak #-}
	pushWeak vImg vStack ix top
	 = do	let n		= toIndex (extent img) ix
		xDst		<- VM.unsafeRead vImg n
		let xSrc	= img `R.unsafeIndex` ix

		if   xDst == edge None 
		  && xSrc == edge Weak
		 then do
			VM.unsafeWrite vStack top (toIndex (extent img) ix)
			return (top + 1)
			
		 else	return top
	


