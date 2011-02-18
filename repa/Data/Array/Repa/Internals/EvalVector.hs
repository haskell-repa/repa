{-# LANGUAGE BangPatterns #-}
module Data.Array.Repa.Internals.EvalVector
	( newVectorP
	, fillVector
	, fillVectorP)
where
import Data.Array.Repa.Internals.Gang
import Data.Vector.Unboxed			as V
import Data.Vector.Unboxed.Mutable		as VM
import System.IO.Unsafe
import GHC.Base					(remInt, quotInt)
import Prelude					as P

-- Vector Filling ---------------------------------------------------------------------------------
newVectorP
	:: Unbox a
	=> (Int -> a)
	-> Int		-- size
	-> Vector a
	
{-# INLINE newVectorP #-}
newVectorP !getElemNew !size
 = unsafePerformIO
 $ do	mvec	<- VM.unsafeNew size
	fillVectorP mvec getElemNew
	V.unsafeFreeze mvec


-- | Fill a vector sequentially.
fillVector
	:: Unbox a
 	=> IOVector a
	-> (Int -> a)
	-> IO ()

{-# INLINE fillVector #-}
fillVector !vec !getElem
 = fill 0
 where 	!len	= VM.length vec
	
	fill !ix
	 | ix >= len	= return ()
	 | otherwise
	 = do	VM.unsafeWrite vec ix (getElem ix)
		fill (ix + 1)


-- | Fill a vector in parallel.
fillVectorP
	:: Unbox a
	=> IOVector a		-- ^ vector to write elements info.
	-> (Int -> a)		-- ^ fn to evaluate an element at a given index.
	-> IO ()
		
{-# INLINE fillVectorP #-}
fillVectorP !vec !getElem
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


