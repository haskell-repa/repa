{-# LANGUAGE PackageImports, FlexibleContexts #-}
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
import Data.Array.Repa.Repr.Unboxed             as A
import Prelude					as P


-- | Read a matrix from a text file.
-- 
--   * WARNING: This is implemented fairly naively, just using `Strings` 
--     under the covers. It will be slow for large data files.
-- 
--   * It also doesn't do graceful error handling.
--     If the file has the wrong format you'll get a confusing `error`.
--
readMatrixFromTextFile
	:: (Num e, Read e, Unbox e)
	=> FilePath
	-> IO (Array U DIM2 e)	

readMatrixFromTextFile fileName
 = do	handle		<- openFile fileName ReadMode
	
	"MATRIX"	<- hGetLine handle
	[width, height]	<- liftM (P.map read . words) $ hGetLine handle
	str		<- hGetContents handle
	let vals	= readValues str

	let dims	= Z :. width :. height
	return $ fromListUnboxed dims vals


-- | Write a matrix as a text file.
writeMatrixToTextFile 
	:: (Show e, Source r e)
	=> FilePath
	-> Array r DIM2 e
	-> IO ()

writeMatrixToTextFile fileName arr
 = do	file	<- openFile fileName WriteMode	

	hPutStrLn file "MATRIX"

	let Z :. width :. height	
		= extent arr

	hPutStrLn file $ show width P.++ " " P.++ show height
	hWriteValues file $ toList arr
	hClose file

