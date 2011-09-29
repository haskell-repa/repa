{-# LANGUAGE BangPatterns, ExplicitForAll, ScopedTypeVariables, PatternGuards #-}
module Data.Array.Repa.Internals.Select
	(selectChunkedS, selectChunkedP)
where
import Data.Array.Repa.Internals.Gang
import Data.Array.Repa.Shape
import Data.Vector.Unboxed			as V
import Data.Vector.Unboxed.Mutable		as VM
import GHC.Base					(remInt, quotInt)
import Prelude					as P
import Control.Monad				as P
import Data.IORef

-- | Select indices matching a predicate
selectChunkedS
	:: (Shape sh, Unbox a)
	=> (sh -> Bool)		-- ^ See if this predicate matches.
	-> (sh -> a)		-- ^  .. and apply fn to the matching index
	-> IOVector a		-- ^  .. then write the result into the vector.
	-> sh 			-- ^ Extent of indices to apply to predicate.
	-> IO Int		-- ^ Number of elements written to destination array.

{-# INLINE selectChunkedS #-}
selectChunkedS match produce !vDst !shSize
 = fill 0 0
 where	lenSrc	= size shSize
	lenDst	= VM.length vDst

	fill !nSrc !nDst
	 | nSrc >= lenSrc	= return nDst
	 | nDst >= lenDst	= return nDst

	 | ixSrc	<- fromIndex shSize nSrc
	 , match ixSrc
	 = do	VM.unsafeWrite vDst nDst (produce ixSrc)
		fill (nSrc + 1) (nDst + 1)

	 | otherwise
	 = 	fill (nSrc + 1) nDst


-- | Select indices matching a predicate, in parallel.
--   The array is chunked up, with one chunk being given to each thread.
--   The number of elements in the result array depends on how many threads
--   you're running the program with.
selectChunkedP
	:: forall a
	.  Unbox a
	=> (Int -> Bool)	-- ^ See if this predicate matches.
	-> (Int -> a)		--   .. and apply fn to the matching index
	-> Int			-- Extent of indices to apply to predicate.
	-> IO [IOVector a]	-- Chunks containing array elements.

{-# INLINE selectChunkedP #-}
selectChunkedP !match !produce !len
 = do
	-- Make IORefs that the threads will write their result chunks to.
	-- We start with a chunk size proportial to the number of threads we have,
	-- but the threads themselves can grow the chunks if they run out of space.
	refs	<- P.replicateM threads
		$ do	vec	<- VM.new $ len `div` threads
			newIORef vec

	-- Fire off a thread to fill each chunk.
	gangIO theGang
	 $ \thread -> makeChunk (refs !! thread)
			(splitIx thread)
			(splitIx (thread + 1) - 1)

	-- Read the result chunks back from the IORefs.
	-- If a thread had to grow a chunk, then these might not be the same ones
	-- we created back in the first step.
	P.mapM readIORef refs

 where	-- See how many threads we have available.
	!threads 	= gangSize theGang
	!chunkLen 	= len `quotInt` threads
	!chunkLeftover	= len `remInt`  threads


	-- Decide where to split the source array.
	{-# INLINE splitIx #-}
	splitIx thread
	 | thread < chunkLeftover = thread * (chunkLen + 1)
	 | otherwise		  = thread * chunkLen  + chunkLeftover


	-- Fill the given chunk with elements selected from this range of indices.
	makeChunk :: IORef (IOVector a) -> Int -> Int -> IO ()
	makeChunk !ref !ixSrc !ixSrcEnd
	 = do	vecDst	<- VM.new (len `div` threads)
		vecDst'	<- fillChunk ixSrc ixSrcEnd vecDst 0 (VM.length vecDst - 1)
		writeIORef ref vecDst'


	-- The main filling loop.
	fillChunk :: Int -> Int -> IOVector a -> Int -> Int -> IO (IOVector a)
	fillChunk !ixSrc !ixSrcEnd !vecDst !ixDst !ixDstEnd
         -- If we've finished selecting elements, then slice the vector down
         -- so it doesn't have any empty space at the end.
	 | ixSrc >= ixSrcEnd
	 = 	return	$ VM.slice 0 ixDst vecDst

	 -- If we've run out of space in the chunk then grow it some more.
	 | ixDst >= ixDstEnd
	 = do	let ixDstEnd'	= VM.length vecDst * 2 - 1
		vecDst' 	<- VM.grow vecDst (ixDstEnd + 1)
		fillChunk (ixSrc + 1) ixSrcEnd vecDst' (ixDst + 1) ixDstEnd'

	 -- We've got a maching element, so add it to the chunk.
	 | match ixSrc
	 = do	VM.unsafeWrite vecDst ixDst (produce ixSrc)
		fillChunk (ixSrc + 1) ixSrcEnd vecDst (ixDst + 1)  ixDstEnd

	 -- The element doesnt match, so keep going.
	 | otherwise
	 =	fillChunk (ixSrc + 1) ixSrcEnd vecDst ixDst ixDstEnd

