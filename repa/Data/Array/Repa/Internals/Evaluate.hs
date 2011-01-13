{-# LANGUAGE BangPatterns #-}
module Data.Array.Repa.Internals.Evaluate
	( fillVector
	, fillVectorP,          newVectorP
	, fillVectorBlockwiseP, newVectorBlockwiseP
	, fillVectorBlock)
where
import Data.Array.Repa.Internals.Gang
import Data.Vector.Unboxed			as V
import Data.Vector.Unboxed.Mutable		as VM
import System.IO.Unsafe
import GHC.Base					(remInt, quotInt)
import GHC.Conc					(numCapabilities)
import Prelude					as P

-- TheGang ----------------------------------------------------------------------------------------
-- | The gang is shared by all computations.
theGang :: Gang
{-# NOINLINE theGang #-}
theGang = unsafePerformIO $ forkGang numCapabilities


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


-- Blockwise filling ------------------------------------------------------------------------------
newVectorBlockwiseP
	:: Unbox a
	=> (Int -> a)
	-> Int		-- size
	-> Int		-- width
	-> Vector a
	
{-# INLINE newVectorBlockwiseP #-}
newVectorBlockwiseP !getElemNew !size !width
 = unsafePerformIO
 $ do	mvec	<- VM.unsafeNew size
	fillVectorBlockwiseP mvec getElemNew width
	V.unsafeFreeze mvec
	
				
fillVectorBlockwiseP 
	:: Unbox a
	=> IOVector a		-- ^ vector to write elements into
	-> (Int -> a)		-- ^ fn to evaluate an element at the given index
	-> Int			-- ^ width of image.
	-> IO ()
	
{-# INLINE fillVectorBlockwiseP #-}
fillVectorBlockwiseP !vec !getElemFVBP !imageWidth 
 = 	gangIO theGang fillBlock
	
 where	!threads	= gangSize theGang
	!vecLen		= VM.length vec
	!imageHeight	= vecLen `div` imageWidth
	!colChunkLen	= imageWidth `quotInt` threads
	!colChunkSlack	= imageWidth `remInt`  threads
	
	{-# INLINE colIx #-}
	colIx !ix
	 | ix < colChunkSlack 	= ix * (colChunkLen + 1)
	 | otherwise		= ix * colChunkLen + colChunkSlack

	
	-- just give one column to each thread
	{-# INLINE fillBlock #-}
	fillBlock :: Int -> IO ()
	fillBlock !ix 
	 = let	!x0	= colIx ix
		!x1	= colIx (ix + 1)
		!y0	= 0
		!y1	= imageHeight
	   in	fillVectorBlock vec getElemFVBP imageWidth x0 y0 x1 y1


fillVectorBlock
	:: Unbox a
	=> IOVector a		-- ^ vector to write elements into.
	-> (Int -> a)		-- ^ fn to evaluate an element at the given index.
	-> Int			-- ^ width of whole image
	-> Int			-- ^ x0 
	-> Int			-- ^ y0
	-> Int			-- ^ x1
	-> Int			-- ^ y1
	-> IO ()

{-# INLINE fillVectorBlock #-}
fillVectorBlock !vec !getElemFVB !imageWidth !x0 !y0 !x1 !y1
 = fillBlock ixStart (ixStart + (x1 - x0))
 where	
	-- offset from end of one line to the start of the next.
	!ixStart	= x0 + y0 * imageWidth
	!ixFinal	= x1 + y1 * imageWidth
	
	{-# INLINE fillBlock #-}
	fillBlock !ixLineStart !ixLineEnd
	 | ixLineStart >= ixFinal	= return ()
	 | otherwise
	 = do	fillLine ixLineStart
		fillBlock (ixLineStart + imageWidth) (ixLineEnd + imageWidth)
	
	 where	{-# INLINE fillLine #-}
		fillLine !ix
 	   	 | ix >= ixLineEnd	= return ()
	   	 | otherwise
	   	 = do	VM.unsafeWrite vec ix (getElemFVB ix)
			fillLine (ix + 1)
