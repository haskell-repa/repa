
-- | Reading and writing matricies as ASCII files.
--	We use ASCII so we can generate and check simple test data by hand,
--	and we don't want to fool around with byte order issues.
--
--   Matrix file format is like:
--
--	MATRIX			-- header
--	100 100			-- width and height
--	1.23 1.56 1.23 ...	-- data, separated by whitespace
--	....
--
-- TODO: Merge this with PPM.hs
-- TODO: Merge this with Vector.hs. 
--	 We should really have fns that read and write arrays of arbitrary dimension.
--
module Matrix
	( readMatrixFromTextFile
	, writeMatrixAsTextFile
	, genRandomMatrix)
where
import Data.List				as L
import Data.Array.Repa				as A
import qualified Data.Array.Parallel.Unlifted	as U
import Prelude					as P
import System.IO
import Control.Monad
import Data.Char
import System.Random


-- Reading ----------------------------------------------------------------------------------------
-- | Read a matrix from a text file.
readMatrixFromTextFile
	:: (U.Elt a, Num a, Read a)
	=> FilePath			-- ^ File name of matrix file.
	-> IO (Array DIM2 a)	

readMatrixFromTextFile fileName
 = do	handle		<- openFile fileName ReadMode
	
	"MATRIX"	<- hGetLine handle
	[width, height]	<- liftM (P.map read . words) $ hGetLine handle
	str		<- hGetContents handle
	let vals	= readValues str

	let dims	= Z :. width :. height
	let mat		= fromList dims vals

	return mat



-- | Write a matrix as a text file.
writeMatrixAsTextFile 
	:: (U.Elt a, Show a)
	=> Array DIM2 a			-- ^ Matrix to write.
	-> FilePath			-- ^ File name of output file.
	-> IO ()

writeMatrixAsTextFile arr fileName
 = do	file	<- openFile fileName WriteMode	

	hPutStrLn file "MATRIX"

	let Z :. width :. height	
		= extent arr

	hPutStrLn file $ show width ++ " " ++ show height
		
	hWriteValues file $ toList arr
	hClose file


-- | Generate a random(ish) matrix.
genRandomMatrix 
	:: DIM2 
	-> IO (Array DIM2 Double)

genRandomMatrix sh
 = do	uarr	<- genRandomUArray (A.size sh)
	return	$ fromUArray sh uarr



