{-# LANGUAGE PackageImports #-}
-- | Read and write matrices as ASCII text files.
--
--   The file format is like:
--
--   @
--	MATRIX			-- header
--	100 100			-- width and height
--	1.23 1.56 1.23 ...	-- data, separated by whitespace
--	....
--   @
module Data.Array.Repa.IO.Matrix
	( readMatrixFromTextFile
	, writeMatrixToTextFile)
where
import Data.Array.Repa.IO.Internals.Text
import Control.Monad
import System.IO
import Data.List				as L
import Data.Array.Repa				as A
import Prelude					as P
import qualified "dph-prim-par" Data.Array.Parallel.Unlifted	as U


-- | Read a matrix from a text file.
--
--   WARNING: This doesn't do graceful error handling. If the file has the wrong format
--   you'll get a confusing `error`.
readMatrixFromTextFile
	:: (U.Elt a, Num a, Read a)
	=> FilePath
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
writeMatrixToTextFile 
	:: (U.Elt a, Show a)
	=> FilePath
	-> Array DIM2 a
	-> IO ()

writeMatrixToTextFile fileName arr
 = do	file	<- openFile fileName WriteMode	

	hPutStrLn file "MATRIX"

	let Z :. width :. height	
		= extent arr

	hPutStrLn file $ show width ++ " " ++ show height
		
	hWriteValues file $ toList arr
	hClose file

