-- | Evaluate an array by breaking it up into linear chunks and filling
--   each chunk in parallel.
module Data.Array.Repa.Eval.Chunked
	( fillChunkedP
	, fillChunkedS)
where
import Data.Array.Repa.Eval.Gang
import GHC.Base		(remInt, quotInt)
import Prelude		as P

-- | Fill something sequentially.
-- 
--   * The array is filled linearly from start to finish.  
-- 
fillChunkedS
	:: Int                  -- ^ Number of elements.
	-> (Int -> a -> IO ())	-- ^ Update function to write into result buffer.
	-> (Int -> a)	        -- ^ Fn to get the value at a given index.
	-> IO ()

{-# INLINE [0] fillChunkedS #-}
fillChunkedS !len !write !getElem
 = fill 0
 where	fill !ix
	 | ix >= len	= return ()
	 | otherwise
	 = do	write ix (getElem ix)
		fill (ix + 1)


-- | Fill something in parallel.
-- 
--   * The array is split into linear chunks and each thread fills one chunk.
-- 
fillChunkedP
        :: Int                  -- ^ Number of elements.
	-> (Int -> a -> IO ())	-- ^ Update function to write into result buffer.
	-> (Int -> a)	        -- ^ Fn to get the value at a given index.
	-> IO ()

{-# INLINE [0] fillChunkedP #-}
fillChunkedP !len !write !getElem
 = 	gangIO theGang
	 $  \thread -> fill (splitIx thread) (splitIx (thread + 1))

 where
	-- Decide now to split the work across the threads.
	-- If the length of the vector doesn't divide evenly among the threads,
	-- then the first few get an extra element.
	!threads 	= gangSize theGang
	!chunkLen 	= len `quotInt` threads
	!chunkLeftover	= len `remInt`  threads

	{-# INLINE splitIx #-}
	splitIx thread
	 | thread < chunkLeftover = thread * (chunkLen + 1)
	 | otherwise		  = thread * chunkLen  + chunkLeftover

	-- Evaluate the elements of a single chunk.
	{-# INLINE fill #-}
	fill !ix !end
	 | ix >= end		= return ()
	 | otherwise
	 = do	write ix (getElem ix)
		fill (ix + 1) end

