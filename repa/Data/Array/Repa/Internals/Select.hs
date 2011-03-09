{-# LANGUAGE BangPatterns, ExplicitForAll, ScopedTypeVariables #-}

module Data.Array.Repa.Internals.Select
	(selectChunkedS, selectChunkedP)
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Internals.Elt
import Data.Array.Repa.Internals.Gang
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



selectChunkedP 
	:: forall a
	.  Unbox a
	=> (Int -> Bool)	-- ^ See if this predicate matches.
	-> (Int -> a)		--   .. and apply fn to the matching index
	-> Int			-- Extent of indices to apply to predicate.
	-> IO [IOVector a]	-- Chunks containing array elements.

{-# INLINE selectChunkedP #-}
selectChunkedP !match !produce !len
 = do	refs	<- P.replicateM threads 
		$ do	vec	<- VM.new $ len `div` threads
			newIORef vec

	gangIO theGang
	 $ \thread -> makeChunk (refs !! thread) (splitIx thread) (splitIx (thread + 1) - 1)
	
	P.mapM readIORef refs
	
 where	!threads 	= gangSize theGang
	!chunkLen 	= len `quotInt` threads
	!chunkLeftover	= len `remInt`  threads

	-- Decide where to split the source array.
	{-# INLINE splitIx #-}
	splitIx thread
	 | thread < chunkLeftover = thread * (chunkLen + 1)
	 | otherwise		  = thread * chunkLen  + chunkLeftover

	-- Select elements from the source array and fill the given vector.
	makeChunk :: IORef (IOVector a) -> Int -> Int -> IO ()
	makeChunk !ref !ixSrc !ixSrcEnd
	 = do	vecDst	<- VM.new (len `div` threads)
		vecDst'	<- fillChunk ixSrc ixSrcEnd vecDst 0 (VM.length vecDst - 1)		
		writeIORef ref vecDst'
	
	fillChunk :: Int -> Int -> IOVector a -> Int -> Int -> IO (IOVector a)
	fillChunk !ixSrc !ixSrcEnd !vecDst !ixDst !ixDstEnd
	 | ixSrc >= ixSrcEnd	
	 = 	return	$ VM.slice 0 ixDst vecDst
		
	 | ixDst >= ixDstEnd
	 = do	let ixDstEnd'	= VM.length vecDst * 2 - 1
		vecDst' 	<- VM.grow vecDst (ixDstEnd + 1)
		fillChunk (ixSrc + 1) ixSrcEnd vecDst' (ixDst + 1) ixDstEnd'
	 
	 | match ixSrc
	 = do	VM.unsafeWrite vecDst ixDst (produce ixSrc)
		fillChunk (ixSrc + 1) ixSrcEnd vecDst (ixDst + 1)  ixDstEnd
		
	 | otherwise
	 =	fillChunk (ixSrc + 1) ixSrcEnd vecDst ixDst ixDstEnd

