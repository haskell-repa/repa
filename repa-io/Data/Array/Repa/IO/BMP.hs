{-# LANGUAGE PackageImports, PatternGuards, ExplicitForAll  #-} 

-- | Reading and writing arrays as uncompressed 24 and 32 bit Windows BMP files.
module Data.Array.Repa.IO.BMP
	( readImageFromBMP
	, readComponentsFromBMP
	, readComponentsListFromBMP
	, readMatrixFromGreyscaleBMP

	-- Writing.
	, writeImageToBMP
	, writeComponentsToBMP
	, writeComponentsListToBMP
	, writeMatrixToGreyscaleBMP)
where
import Data.Array.Repa				as A
import Data.Array.Repa.ByteString               as A
import Prelude					as P
import Codec.BMP
import Data.Word

-- NOTE: We set most of these functions as NOINLINE so it's easier to understand
--       what's going on in the core programs. The top-level IO functions are
--       only called a few times each, so it doesn't matter if they're not
--       worker/wrappered etc.
	
-- Read -------------------------------------------------------------------------------------------
-- | Read a matrix from a `BMP` file.
--	Each pixel is converted to greyscale, normalised to [0..1] and used
--	as the corresponding array element. If anything goes wrong when loading the file then `Error`.
readMatrixFromGreyscaleBMP
	:: FilePath
	-> IO (Either Error (Array DIM2 Double))

{-# NOINLINE readMatrixFromGreyscaleBMP #-}
readMatrixFromGreyscaleBMP filePath
 = do	eComps	<- readComponentsFromBMP filePath
	case eComps of 
	 Left err	-> return $ Left err
  	 Right (arrRed, arrGreen, arrBlue)
 	  -> let arr	= force2 
			$ A.fromFunction (extent arrRed)
			   (\ix -> sqrt ( (fromIntegral (arrRed   ! ix) / 255) ^ (2 :: Int)
					+ (fromIntegral (arrGreen ! ix) / 255) ^ (2 :: Int)
					+ (fromIntegral (arrBlue  ! ix) / 255) ^ (2 :: Int)))
	     in	arr `deepSeqArray` return (Right arr)


-- | Like `readComponentsFromBMP`, but return the components as a list.
readComponentsListFromBMP
	:: FilePath
	-> IO (Either Error [Array DIM2 Word8])

readComponentsListFromBMP filePath
 = do	eComps	<- readComponentsFromBMP filePath
	case eComps of
	 Left err
	  -> return $ Left err

	 Right (arrRed, arrGreen, arrBlue)	
	  -> return $ Right [arrRed, arrGreen, arrBlue]


-- | Read RGB components from a BMP file.
--	Returns arrays of red, green and blue components, all with the same extent.
--	If anything goes wrong when loading the file then then `Error`.
readComponentsFromBMP
	:: FilePath
	-> IO (Either Error (Array DIM2 Word8, Array DIM2 Word8, Array DIM2 Word8))

{-# NOINLINE readComponentsFromBMP #-}
readComponentsFromBMP filePath
 = do	ebmp	<- readBMP filePath
	case ebmp of
	 Left err	-> return $ Left err
	 Right bmp	-> return $ Right (readComponentsFromBMP' bmp)

readComponentsFromBMP' bmp
 = let	(width, height) = bmpDimensions bmp

	arr		= A.fromByteString (Z :. height :. width * 4)
			$ unpackBMPToRGBA32 bmp

	shapeFn _ 	= Z :. height :. width

	arrRed	
	 = force2 $ traverse arr shapeFn
		(\get (sh :. x) -> get (sh :. (x * 4)))

	arrGreen
	 = force2 $ traverse arr shapeFn
		(\get (sh :. x) -> get (sh :. (x * 4 + 1)))

	arrBlue
	 = force2 $ traverse arr shapeFn
		(\get (sh :. x) -> get (sh :. (x * 4 + 2)))
	
   in	[arrRed, arrGreen, arrBlue] `deepSeqArrays` (arrRed, arrGreen, arrBlue)


-- | Read a RGBA image from a BMP file.
--	In the result, the higher two dimensions are the height and width,
--	and the lower indexes the RGBA component of each pixel. 
--      If the BMP read has no alpha channel then alpha of the resulting pixels is set to 255.
--	If anything goes wrong when loading the file then `Error`.
readImageFromBMP 
	:: FilePath
	-> IO (Either Error (Array DIM3 Word8))

{-# NOINLINE readImageFromBMP #-}
readImageFromBMP filePath
 = do	ebmp	<- readBMP filePath
	case ebmp of
	 Left err	-> return $ Left err
	 Right bmp	-> return $ Right (readImageFromBMP' bmp)
	
readImageFromBMP' bmp
 = let	(width, height)	= bmpDimensions bmp
	arr		= fromByteString (Z :. height :. width :. 4)
			$ unpackBMPToRGBA32 bmp
   in	arr



-- Write ------------------------------------------------------------------------------------------
-- | Write a matrix to a BMP file.
--	Negative values are discarded. Positive values are normalised to the maximum 
--	value in the matrix and used as greyscale pixels.
writeMatrixToGreyscaleBMP 
	:: forall a. (Num a, Elt a, Fractional a, RealFrac a)
	=> FilePath
	-> Array DIM2 a
	-> IO ()

{-# NOINLINE   writeMatrixToGreyscaleBMP #-}
{-# SPECIALISE writeMatrixToGreyscaleBMP :: FilePath -> Array DIM2 Float  -> IO () #-}
{-# SPECIALISE writeMatrixToGreyscaleBMP :: FilePath -> Array DIM2 Double -> IO () #-}
writeMatrixToGreyscaleBMP fileName arr
 = let	arrNorm		= normalisePositive01 arr
	scale x		= fromIntegral (truncate (x * 255) :: Int)
	arrWord8	= A.map scale arrNorm
   in	writeComponentsToBMP fileName arrWord8 arrWord8 arrWord8
		
		
-- | Like `writeComponentsToBMP` but take the components as a list.
--   The list must have 3 arrays, for the red, green blue components
--   respectively, else `error`.
writeComponentsListToBMP
	:: FilePath 
	-> [Array DIM2 Word8]
	-> IO ()

writeComponentsListToBMP filePath comps
	| [red, green, blue]	<- comps
	= writeComponentsToBMP filePath red green blue
	
	| otherwise
	= error "Data.Array.Repa.IO.BMP.writeComponentsListToBMP: wrong number of components"
	

-- | Write RGB components to a BMP file.
--	All arrays must have the same extent, else `error`.
writeComponentsToBMP
	:: FilePath
	-> Array DIM2 Word8
	-> Array DIM2 Word8
	-> Array DIM2 Word8
	-> IO ()

{-# NOINLINE writeComponentsToBMP #-}
writeComponentsToBMP fileName arrRed arrGreen arrBlue
 | not $ (  extent arrRed   == extent arrGreen       
         && extent arrGreen == extent arrBlue)
 = error "Data.Array.Repa.IO.BMP.writeComponentsToBMP: arrays don't have same extent"

 | otherwise
 = do	let Z :. height :. width	
			= extent arrRed
		
	-- Build image data from the arrays.
	let arrAlpha	= fromFunction (extent arrRed) (\_ -> 255)
	let arrRGBA	= interleave4 arrRed arrGreen arrBlue arrAlpha
	let bmp		= packRGBA32ToBMP width height
			$ A.toByteString arrRGBA
	
	writeBMP fileName bmp


-- | Write a RGBA image to a BMP file.
--	The higher two dimensions are the height and width of the image. 
--	The lowest dimension must have size 4, corresponding to the RGBA components
--	of each pixel, else `error`. 
writeImageToBMP 
	:: FilePath
	-> Array DIM3 Word8
	-> IO ()

{-# NOINLINE writeImageToBMP #-}
writeImageToBMP fileName arrImage
	| comps /= 4
	= error "Data.Array.Repa.IO.BMP: lowest order dimension must be 4"

	| otherwise
	= let 	bmp	= packRGBA32ToBMP width height
			$ A.toByteString arrImage
	  in	writeBMP fileName bmp
	
	where	Z :. height :. width :. comps	
			= extent arrImage
	

-- Normalise --------------------------------------------------------------------------------------
-- | Normalise a matrix to to [0 .. 1], discarding negative values.
--	If the maximum value is 0 then return the array unchanged.
normalisePositive01
	:: (Shape sh, Elt a, Fractional a, Ord a)
	=> Array sh a
	-> Array sh a

{-# INLINE normalisePositive01 #-}
normalisePositive01 arr	
 = let	mx		= foldAll max 0 arr
   	elemFn x
	 | x >= 0	= x / mx
	 | otherwise	= x
   in	mx `seq`
	 if mx == 0 
	  then arr
	  else A.map elemFn arr

