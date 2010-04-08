{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

-- | Writing out matricies as PPM image files.
module PPM
	( writeMatrixAsNormalisedPPM
	, writeMatrixAsPPM
	, readPPMAsMatrix
	, readPPMAsMatrix2)
where
import qualified Data.Array.Parallel.Unlifted 	as U
import Data.List				as L
import Array					as A
import Prelude					as P
import System.IO
import Control.Monad
import Data.Char
import Debug.Trace
import Data.ByteString.Char8			(ByteString)
import qualified Data.ByteString.Char8		as BC


-- Write ------------------------------------------------------------------------------------------
-- | Convert a matrix to a PPM image,
--	while normalising it to the maximum value present in the matrix.
writeMatrixAsNormalisedPPM 
	:: FilePath				-- ^ Filename of output file.
	-> (Double -> (Double, Double, Double))	-- ^ Function for producing colors from data values,
	-> Array DIM2 Double			-- ^ Matrix of values (need not be normalised).
	-> IO ()

writeMatrixAsNormalisedPPM fileName colorFn arr
 = let	-- Use the maximum elem in the array as the white value.
	vals	= U.toList $ fromArray arr
	maxVal	= maximum vals

	-- Normalise the array to the range [0..1] for display.
	arrNorm	= A.map (/ maxVal) arr

  in	writeMatrixAsPPM fileName colorFn arrNorm


-- | Convert a matrix to a PPM image.
--	Matrix elements should be normalised to [0..1]
writeMatrixAsPPM 
	:: FilePath
	-> (Double -> (Double, Double, Double))	-- ^ Function for producing colors from data values.
	-> Array DIM2 Double 			-- ^ Matrix of values, normalised to [0..1]
	-> IO ()

writeMatrixAsPPM fileName colorFn arr
 = let		
	-- Break flat array data into individual rows
	() :. width :. height	 
		= arrayShape arr

	-- PPM header for pixmap image
	header	= "P3"

   in do
	file	<- openFile fileName WriteMode
	hPutStrLn file $ "P3"
	hPutStrLn file $ show width ++ " " ++ show height
	hPutStrLn file $ "255"
	hWritePixels file colorFn $ U.toList $ fromArray arr
	hClose file


-- | Write out pixel values to a file.
hWritePixels 
	:: Handle 
	-> (Double -> (Double, Double, Double))	-- ^ Function for producing colors from data values.
	-> [Double] 				-- ^ Data values.
	-> IO ()

hWritePixels h colorFn !xx
 = go xx
 where
	go []		= return ()
	go (x:xs)
	 = do	let (r, g, b)	= colorFn x
		hPutStr h $ showInt $ truncate (r * 255)
		hPutStr h $ " "
		hPutStr h $ showInt $ truncate (g * 255)
		hPutStr h $ " "
		hPutStr h $ showInt $ truncate (b * 255)
		hPutStr h $ "\n"
		go xs
	
showInt :: Int -> String
showInt i	= show i

-- | Break flat list into rows of a given width.
takeRows :: Int -> [a] -> [[a]]
takeRows width []	= []
takeRows width xx	= take width xx : takeRows width (drop width xx)

	
-- | Pad a string into a right justified column of a given width.
padR :: Int -> String -> String
padR width str	= L.replicate (width - length str) ' ' ++ str


-- Read -------------------------------------------------------------------------------------------
readPPMAsMatrix 
	:: (Int -> Int -> Int -> Double)	-- ^ Function for producing array values from RGB pixel values
	-> FilePath				-- ^ File name of ppm file.
	-> IO (Array DIM2 Double)		-- ^ Loaded matrix.
	
readPPMAsMatrix pointFn fileName
 = do	file	<- openFile fileName ReadMode
	
	"P3"		<- hGetLine file
	[width, height]	<- liftM (P.map read . words) $ hGetLine file
	(maxVal :: Int)	<- liftM read $ hGetLine file

	vals		<- loadPixels pointFn file

	let dim	= () :. width :. height
	let mat	= toArray dim $ U.fromList vals

	return mat


-- | Read the values of two separate matricies that are encoded in a single PPM file.
readPPMAsMatrix2 
	:: (Int -> Int -> Int -> (Double, Double))	
					-- ^ Function for producing array values from RGB pixel values
	-> FilePath			-- ^ File name of ppm file.
	-> IO   ( Array DIM2 Double	-- ^ Loaded matrix.
		, Array DIM2 Double)
		
readPPMAsMatrix2 pointFn fileName
 = do	file	<- openFile fileName ReadMode
	
	"P3"		<- hGetLine file
	[width, height]	<- liftM (P.map read . words) $ hGetLine file
	(maxVal :: Int)	<- liftM read $ hGetLine file

	vals		<- loadPixels pointFn file

	let dim	= () :. width :. height
	let mat1	= toArray dim $ U.fromList $ P.map fst vals
	let mat2	= toArray dim $ U.fromList $ P.map snd vals

	return (mat1, mat2)
	

-- | Load list of array data from the data part of a PPM file.
loadPixels 
	:: (Int -> Int -> Int -> a)	-- ^ Function for producing array values from RGB pixel values.
	-> Handle 			-- ^ Handle of file to load from.
	-> IO [a]
	
loadPixels pointFn handle
 = do	str		<- hGetContents handle
	let ints	= readInts str
	let vals	= convertLine pointFn ints
	return vals


-- | Convert groups of RGB components to values.
convertLine 
	:: (Int -> Int -> Int -> a)	-- ^ Function for producing array values from RGB pixel values.
	-> [Int] 			-- ^ Ints read from the PPM file.
	-> [a]				-- ^ Output values.

convertLine fn vs
 = case vs of
	[]			-> []
	r : g : b : rest	-> fn r g b : convertLine fn rest

	
-- | Read a string containing ints separated by whitespace.	
readInts :: String -> [Int]
readInts cs	= readInts' [] cs
 where	readInts' _ []	= []
	readInts' acc (c : rest)
		| isSpace c
		= if null acc 
			then readInts' [] rest
			else read (reverse acc) : readInts' [] rest

		| isDigit c
		= readInts' (c : acc) rest

		| otherwise
		= error $ "unexpected char in PPM file " ++ show (ord c)

