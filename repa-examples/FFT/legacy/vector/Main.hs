{-# LANGUAGE BangPatterns #-}

import FFT
import Data.List
import Data.Complex
import Codec.BMP
import System.Environment

import qualified Data.ByteString	as BS
import Data.ByteString			(ByteString)
import qualified Data.ByteString.Unsafe	as BS
import qualified Data.Vector.Unboxed	as U
import Data.Vector.Unboxed		(Vector)
import Data.Word

main 
 = do	args	<- getArgs
	case args of
	 [fileNameIn, fileNameOut]	
	   -> run fileNameIn fileNameOut
	 _ -> putStr $ "usage: highpass <fileIn.bmp> <fileOut.bmp>"
			

run :: String -> String -> IO ()
run fileNameIn fileNameOut
 = do	
	Right bmp		<- readBMP fileNameIn
	let bsRGBA		= unpackBMPToRGBA32 bmp
	let (width, height)	= bmpDimensions bmp
	let size		= width * height
	
	-- Separate the components of the image.
	let vecRed	= takeComponent size 0 bsRGBA
	let vecGreen	= takeComponent size 1 bsRGBA
	let vecBlue	= takeComponent size 2 bsRGBA
				
	-- Transform each component.
	let vecRed'	= transform width height vecRed
	let vecGreen'	= transform width height vecGreen
	let vecBlue'	= transform width height vecBlue
	
	-- Write the image back to a file.
	writeBMP fileNameOut
		$ makeBMP width height vecRed' vecGreen' vecBlue'


-- | Take one component of an RGBA image as a vector.
takeComponent :: Int -> Int -> ByteString -> U.Vector Word8 
takeComponent size comp vec
	=  U.generate size (\ix -> vec `BS.unsafeIndex` (ix * 4 + comp))
	

-- | Do high pass filtering on an image.
transform :: Int -> Int -> U.Vector Word8 -> U.Vector Word8
transform width height vec
 = let	freq	= fft2d Forward width height 	
		$ U.map (\x -> (fromIntegral x) :+ 0) vec
	
	freq'	= U.update freq (U.fromList [(0, 0)])
			
	vec'	= U.map (truncate . magnitude) 	
		$ fft2d Inverse width height freq'
		
   in	vec'


-- | Build a BMP image from vectors of its components.
makeBMP 
	:: Int -> Int 
	-> U.Vector Word8
	-> U.Vector Word8
	-> U.Vector Word8
	-> BMP

makeBMP width height vecRed vecGreen vecBlue
 = vecRed `seq` vecGreen `seq` vecBlue `seq`
   let	{-# INLINE getElem #-}
        getElem	ix 
 	 = case ix `mod` 4 of
		0	-> vecRed   `U.unsafeIndex` (ix `div` 4)
		1	-> vecGreen `U.unsafeIndex` (ix `div` 4)
		2	-> vecBlue  `U.unsafeIndex` (ix `div` 4)
		3	-> 0

   in 	packRGBA32ToBMP width height
		$ fst $ BS.unfoldrN 
			(width * height * 4) 
			(\ix -> if ix == width * height * 4
					then Nothing
					else Just (getElem ix, ix + 1))
			0


