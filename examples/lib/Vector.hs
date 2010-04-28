
-- | Reading and writing vectors as ASCII files.
--	We use ASCII so we can generate and check simple test data by hand,
--	and we don't want to fool around with byte order issues.
--
--   Vector file format is like:
--
--	VECTOR			-- header
--	100			-- length of vector
--	1.23 1.56 1.23 ...	-- data, separated by whitespace
--	....
--
--
module Vector
	( readVectorFromTextFile
	, writeVectorAsTextFile
	, genRandomVector

	, readValues
	, hWriteValues
	, genRandomUArray)
where
import Data.List				as L
import Data.Array.Repa				as A
import qualified Data.Array.Parallel.Unlifted	as U
import Prelude					as P
import System.IO
import Control.Monad
import Data.Char
import System.Random


-- | Read a vector from a text file.
readVectorFromTextFile
	:: (U.Elt a, Num a, Read a)
	=> FilePath			-- ^ File name of vector file.
	-> IO (Array DIM1 a)	

readVectorFromTextFile fileName
 = do	handle		<- openFile fileName ReadMode
	
	"VECTOR"	<- hGetLine handle
	[len]		<- liftM (P.map readInt . words) $ hGetLine handle
	str		<- hGetContents handle
	let vals	= readValues str

	let dims	= Z :. len
	let vec		= fromList dims vals

	return vec

readInt :: String -> Int
readInt str
	| and $ P.map isDigit str
	= read str
	
	
-- | Write a vector as a text file.
writeVectorAsTextFile 
	:: (U.Elt a, Show a)
	=> Array DIM1 a			-- ^ Vector to write.
	-> FilePath			-- ^ File name of output file.
	-> IO ()

writeVectorAsTextFile arr fileName
 = do	file	<- openFile fileName WriteMode	

	hPutStrLn file "VECTOR"

	let Z :. len
		= extent arr

	hPutStrLn file 	$ show len
	hWriteValues file $ toList arr
	hClose file
	

-- | Generate a random(ish) vector.
genRandomVector 
	:: DIM2 
	-> IO (Array DIM2 Double)

genRandomVector sh
 = do	uarr	<- genRandomUArray (A.size sh)
	return	$ fromUArray sh uarr
			


-- Stuff shared with Matrix module -------------------------------------------------------------
-- | Write out values to a file.
hWriteValues
	:: Show a
	=> Handle 
	-> [a] 				-- ^ Data values.
	-> IO ()

hWriteValues handle xx
 = go xx
 where
	go []		= return ()
	go (x:xs)
	 = do	hPutStr handle $ show x
		hPutStr handle $ "\n"
		go xs


-- | Read a string containing ints separated by whitespace.	
readValues :: (Num a, Read a) => String -> [a]
readValues cs	= readValues' [] cs
 where	readValues' _ []	= []
	readValues' acc (c : rest)
		| isSpace c
		= if null acc 
			then readValues' [] rest
			else read (reverse acc) : readValues' [] rest

		| isDigit c || c == '.' || c == 'e' || c == '-'
		= readValues' (c : acc) rest

		| otherwise
		= error $ "unexpected char in Matrix file " ++ show (ord c)


-- | Generate a random(ish) UArray of doubles.
-- The std random function is too slow to generate really big vectors
-- with.  Instead, we generate a short random vector and repeat that.
genRandomUArray :: Int -> IO (U.Array Double)
genRandomUArray n 
 = do	let k		= 1000
    	rg		<- newStdGen
    	let randvec	= U.randomRs k (-100, 100) rg
	let vec		= U.map (\i -> randvec U.!: (i `mod` k)) (U.enumFromTo 0 (n-1))
	return vec




