{-# LANGUAGE PackageImports, FlexibleContexts #-}
-- | Read and write vectors as ASCII text files.
--
--   The file format is like:
--
--   @
--	VECTOR			-- header
--	100			-- length of vector
--	1.23 1.56 1.23 ...	-- data, separated by whitespace
--	....
--   @
module Data.Array.Repa.IO.Vector
	( readVectorFromTextFile
	, writeVectorToTextFile)
where
import Data.Array.Repa				as A
import Data.Array.Repa.Repr.Unboxed             as A
import Data.Array.Repa.IO.Internals.Text
import Data.List				as L
import Prelude					as P
import System.IO
import Control.Monad
import Data.Char


-- | Read a vector from a text file.
--
--   * WARNING: This is implemented fairly naively, just using `Strings` 
--     under the covers. It will be slow for large data files.
-- 
--   * It also doesn't do graceful error handling.
--     If the file has the wrong format you'll get a confusing `error`.
--
readVectorFromTextFile
	:: (Num e, Read e, Unbox e)
	=> FilePath
	-> IO (Array U DIM1 e)	

readVectorFromTextFile fileName
 = do	handle		<- openFile fileName ReadMode
	
	"VECTOR"	<- hGetLine handle
	[len]		<- liftM (P.map readInt . words) $ hGetLine handle
	str		<- hGetContents handle
	let vals	= readValues str

	let dims	= Z :. len
	return $ fromListUnboxed dims vals


readInt :: String -> Int
readInt str
	| and $ P.map isDigit str
	= read str
	
	| otherwise
	= error "Data.Array.Repa.IO.Vector.readVectorFromTextFile parse error when reading data"
	
	
-- | Write a vector as a text file.
writeVectorToTextFile 
	:: (Show e, Source r e)
	=> Array r DIM1 e
	-> FilePath
	-> IO ()

writeVectorToTextFile arr fileName
 = do	file	<- openFile fileName WriteMode	

	hPutStrLn file "VECTOR"

	let Z :. len
		= extent arr

	hPutStrLn file 	$ show len
	hWriteValues file $ toList arr
	hClose file
	hFlush file
	
