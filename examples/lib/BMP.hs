
module BMP
	( writeMatrixToGreyscaleBMP
	, writeComponentsToBMP
	, readMatrixFromBMP
	, readComponentsFromBMP)
where
import qualified Data.Array.Parallel.Unlifted 	as U
import Data.List				as L
import Data.Array.Repa				as A
import Prelude					as P
import System.IO
import Control.Monad
import Data.Char
import Data.Word
import Codec.BMP

-- Write ------------------------------------------------------------------------------------------
-- | Write a matrix to a BMP file.
--	Negative values are discarded. Positive values are normalised to the maximum 
--	value in the matrix and used as greyscale pixels.
writeMatrixToGreyscaleBMP 
	:: FilePath			-- ^ Filename of output file.
	-> Array DIM2 Double		-- ^ Matrix of values (need not be normalised).
	-> IO ()

writeMatrixToGreyscaleBMP fileName arr
 = let	arrNorm		= normalisePositive01 arr
	arrWord8	= A.map (fromIntegral . truncate . (*) 255) arrNorm
   in	writeComponentsToBMP fileName arrWord8 arrWord8 arrWord8
		

-- | Write RGB components to a BMP file.
--	All arrays must have the same extent, else error.
writeComponentsToBMP
	:: FilePath
	-> Array DIM2 Word8		-- ^ Red   components.
	-> Array DIM2 Word8		-- ^ Green components.
	-> Array DIM2 Word8		-- ^ Blue  components.
	-> IO ()

writeComponentsToBMP fileName arrRed arrGreen arrBlue
 | not $ (  extent arrRed   == extent arrGreen       
         && extent arrGreen == extent arrBlue)
 = error "writeComponentsToBMP: arrays don't have same extent"

 | otherwise
 = do	let Z :. width :. height	
			= extent arrRed
		
	-- Build image data from the arrays.
	let arrAlpha	= fromFunction (extent arrRed) (\x -> 255)
	let arrRGBA	= interleave4 arrRed arrGreen arrBlue arrAlpha
	let bmp		= packRGBA32ToBMP height width 
			$ A.toByteString arrRGBA
	
	writeBMP fileName bmp


-- Read -------------------------------------------------------------------------------------------

-- | Read a BMP file as a matrix.
--	Each pixel is converted to greyscale, normalised to [0..1] and used
--	as the corresponding array element.
readMatrixFromBMP
	:: FilePath 
	-> IO (Array DIM2 Double)

readMatrixFromBMP filePath
 = do	(arrRed, arrGreen, arrBlue)
		<- readComponentsFromBMP filePath
	
	let arr	= force 
		$ A.fromFunction (extent arrRed)
			(\ix -> sqrt 	( (fromIntegral (arrRed   !: ix) / 255) ^ 2
					+ (fromIntegral (arrGreen !: ix) / 255) ^ 2
					+ (fromIntegral (arrBlue  !: ix) / 255) ^ 2))
					
	arr `deepSeqArray` return arr
		

-- | Read RGB components from a BMP file.
readComponentsFromBMP
	:: FilePath 
	-> IO (Array DIM2 Word8, Array DIM2 Word8, Array DIM2 Word8)

{-# INLINE readComponentsFromBMP #-}
readComponentsFromBMP filePath
 = do	Right bmp	<- readBMP filePath
	return	$ readComponentsFromBMP' bmp

readComponentsFromBMP' bmp
 = let	(width, height) = bmpDimensions bmp

	arr		= A.fromByteString (Z :. height :. width * 4)
			$ unpackBMPToRGBA32 bmp

	shapeFn _ 	= Z :. height :. width

	arrRed	
	 = traverse arr shapeFn
		(\get (sh :. x) -> get (sh :. (x * 4)))

	arrGreen
	 = traverse arr shapeFn
		(\get (sh :. x) -> get (sh :. (x * 4 + 1)))

	arrBlue
	 = traverse arr shapeFn
		(\get (sh :. x) -> get (sh :. (x * 4 + 2)))
	
   in	(arrRed, arrGreen, arrBlue)

	
	


-- Normalise --------------------------------------------------------------------------------------
-- | Normalise a matrix to to [0 .. 1], discarding negative values.
--	If the maximum value is 0 then return the array unchanged.
normalisePositive01
	:: (Shape sh, U.Elt a, Fractional a, Ord a)
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

