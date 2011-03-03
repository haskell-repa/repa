-- | Evaluate a vector by breaking it up into linear chunks and filling each chunk
--   in parallel.
{-# LANGUAGE BangPatterns #-}
module Data.Array.Repa.Internals.EvalChunked
	( fillChunkedS
	, fillChunkedP)
where
import Data.Array.Repa.Internals.Elt
import Data.Array.Repa.Internals.Gang
import Data.Vector.Unboxed			as V
import Data.Vector.Unboxed.Mutable		as VM
import GHC.Base					(remInt, quotInt)
import Prelude					as P


-- | Fill a vector sequentially.
fillChunkedS
	:: Elt a
 	=> IOVector a	-- ^ Vector to fill.
	-> (Int -> a)	-- ^ Fn to get the value at a given index.
	-> IO ()

{-# INLINE [0] fillChunkedS #-}
fillChunkedS !vec !getElem
 = fill 0
 where 	!len	= VM.length vec
	
	fill !ix
	 | ix >= len	= return ()
	 | otherwise
	 = do	VM.unsafeWrite vec ix (getElem ix)
		fill (ix + 1)


-- | Fill a vector in parallel.
fillChunkedP
	:: Unbox a
	=> IOVector a	-- ^ Vector to fill.
	-> (Int -> a)	-- ^ Fn to get the value at a given index.
	-> IO ()
		
{-# INLINE [0] fillChunkedP #-}
fillChunkedP !vec !getElem
 = 	gangIO theGang 
	 $  \thread -> fill (splitIx thread) (splitIx (thread + 1))

 where	
	-- Decide now to split the work across the threads.
	-- If the length of the vector doesn't divide evenly among the threads,
	-- then the first few get an extra element.
	!threads 	= gangSize theGang
	!len		= VM.length vec
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
	 = do	VM.unsafeWrite vec ix (getElem ix)
		fill (ix + 1) end

