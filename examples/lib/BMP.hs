
module BMP
	(writeMatrixAsNormalisedGreyscaleBMP)
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
-- | Convert a matrix to a PPM image,
--	while normalising it to the maximum value present in the matrix.
writeMatrixAsNormalisedGreyscaleBMP 
	:: FilePath				-- ^ Filename of output file.
	-> Array DIM2 Double			-- ^ Matrix of values (need not be normalised).
	-> IO ()

writeMatrixAsNormalisedGreyscaleBMP fileName arr
 = let	arrNorm		= normalisePositive01 arr
	arrWord8	= A.map (fromIntegral . truncate . (*) 255) arrNorm
   in	writeComponentsToBMP fileName arrWord8 arrWord8 arrWord8
		

-- | Convert a matrix to a PPM image.
--	Matrix elements should be normalised to [0..1]
writeComponentsToBMP
	:: FilePath
	-> Array DIM2 Word8			-- ^ Red   components.
	-> Array DIM2 Word8			-- ^ Green components.
	-> Array DIM2 Word8			-- ^ Blue  components.
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
	let bmp		= packRGBA32ToBMP height width $ A.toByteString arrRGBA
	
	writeBMP fileName bmp


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

