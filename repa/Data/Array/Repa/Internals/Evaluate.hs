{-# LANGUAGE BangPatterns #-}
module Data.Array.Repa.Internals.Evaluate
	( evalSeqLinear
	, evalParLinear)
where
import Data.Array.Repa.Shape			as S
import Data.Array.Repa.Internals.Gang
import Data.Vector.Unboxed			as V
import Data.Vector.Unboxed.Mutable		as VM
import System.IO.Unsafe
import GHC.Base			(remInt, quotInt)
import GHC.Conc			(numCapabilities)


-- | Evaluate a vector sequentially
evalSeqLinear
	:: (Shape sh, Unbox a)
 	=> sh -> (Int -> a) -> Vector a

{-# INLINE evalSeqLinear #-}
evalSeqLinear sh getElem
 = unsafePerformIO
 $ do	vec	<- unsafeNew (S.size sh)	
	fill vec 0
	V.unsafeFreeze vec

 where 	fill !vec !ix
	 | ix >= VM.length vec	= return ()
	 | otherwise
	 = do	VM.unsafeWrite vec ix (getElem ix)
		fill vec (ix + 1)


-- | The gang is shared by all computations.
theGang :: Gang
theGang = unsafePerformIO $ forkGang numCapabilities


-- | Evaluate a vector in parallel.
evalParLinear
	:: (Shape sh, Unbox a)
	=> sh -> (Int -> a) -> Vector a
	
{-# INLINE evalParLinear #-}
evalParLinear !sh !getElem
 = sh `S.deepSeq`
   unsafePerformIO
 $ do	
	-- Allocate the new vector. 
	-- It starts out uninitialised.
	let !len 	= S.size sh
	vec		<- unsafeNew len

	-- Decide now to split the work across the threads.
	-- If the length of the vector doesn't divide evenly among the threads,
	-- then the first few get an extra element.
 	let !threads 		= gangSize theGang
	let !chunkLen 		= len `quotInt` threads
	let !chunkLeftover	= len `remInt`  threads

	let splitIx thread
		| thread < chunkLeftover = thread * (chunkLen + 1)
		| otherwise		 = thread * chunkLen  + chunkLeftover
		
	-- Fill all the chunks in parallel.
	gangIO theGang 
	 $  \thread 
	 -> fill vec (splitIx thread) (splitIx (thread + 1))

	-- Freeze the vector, promising not to update it anymore.
	V.unsafeFreeze vec
	

 where	-- Evaluate the elements of a single chunk.
	{-# INLINE fill #-}
	fill !vec !ix !end 
	 | ix >= end		= return ()
	 | otherwise
	 = do	VM.unsafeWrite vec ix (getElem ix)
		fill vec (ix + 1) end



