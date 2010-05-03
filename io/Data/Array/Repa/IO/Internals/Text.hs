{-# OPTIONS_HADDOCK hide #-}
module Data.Array.Repa.IO.Internals.Text
	( hWriteValues
	, readValues)
where
import System.IO
import Data.Char

-- Stuff shared with Matrix module -------------------------------------------------------------
-- | Write out values to a file.
hWriteValues
	:: Show a
	=> Handle 
	-> [a] 				-- ^ Data values.
	-> IO ()

hWriteValues handle xx
 = go xx
 where	go []		= return ()
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

