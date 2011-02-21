{-# LANGUAGE BangPatterns #-}
module Data.Array.Repa.Internals.EvalBlockwise
	( newVectorBlockwiseP
	, fillVectorBlockwiseP
	, fillVectorBlock
	, fillVectorBlockP)
where
import Data.Array.Repa.Internals.Gang
import Data.Array.Repa.Internals.Elt
import Data.Vector.Unboxed			as V
import Data.Vector.Unboxed.Mutable		as VM
import System.IO.Unsafe
import GHC.Base					(remInt, quotInt)
import Prelude					as P


-- Blockwise filling ------------------------------------------------------------------------------
newVectorBlockwiseP
	:: Elt a
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
	:: Elt a
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


-- Block filling ----------------------------------------------------------------------------------
-- | Fill a block in a 2D image, in parallel.
--   Coordinates given are of the filled edges of the block.
--   We divide the block into columns, and give one column to each thread.
fillVectorBlockP
	:: Elt a
	=> IOVector a		-- ^ vector to write elements into
	-> (Int -> a)		-- ^ fn to evaluate an element at the given index.
	-> Int			-- ^ width of whole image
	-> Int			-- ^ x0 lower left corner of block to fill
	-> Int			-- ^ y0 (low x and y value)
	-> Int			-- ^ x1 upper right corner of block to fill
	-> Int			-- ^ y1 (high x and y value)
	-> IO ()

{-# INLINE fillVectorBlockP #-}
fillVectorBlockP !vec !getElem !imageWidth !x0 !y0 !x1 !y1
 = 	gangIO theGang fillBlock
 where	!threads	= gangSize theGang
	!blockWidth	= x1 - x0
	
	-- All columns have at least this many pixels.
	!colChunkLen	= blockWidth `quotInt` threads

	-- Extra pixels that we have to divide between some of the threads.
	!colChunkSlack	= blockWidth `remInt` threads
	
	-- Get the starting pixel of a column in the image.
	{-# INLINE colIx #-}
	colIx !ix
	 | ix < colChunkSlack	= x0 + ix * (colChunkLen + 1)
	 | otherwise		= x0 + ix * colChunkLen + colChunkSlack
 
	-- Give one column to each thread
	{-# INLINE fillBlock #-}
	fillBlock :: Int -> IO ()
	fillBlock !ix
	 = let	!x0'	= colIx ix
		!x1'	= colIx (ix + 1)
		!y0'	= y0
		!y1'	= y1
	   in	fillVectorBlock vec getElem imageWidth x0' y0' x1' y1'


-- | Fill a block in a 2D image.
--   Coordinates given are of the filled edges of the block.
fillVectorBlock
	:: Elt a
	=> IOVector a		-- ^ vector to write elements into.
	-> (Int -> a)		-- ^ fn to evaluate an element at the given index.
	-> Int			-- ^ width of whole image
	-> Int			-- ^ x0 lower left corner of block to fill 
	-> Int			-- ^ y0 (low x and y value)
	-> Int			-- ^ x1 upper right corner of block to fill
	-> Int			-- ^ y1 (high x and y value)
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
	 | ixLineStart > ixFinal	= return ()
	 | otherwise
	 = do	fillLine4 ixLineStart
		fillBlock (ixLineStart + imageWidth) (ixLineEnd + imageWidth)
	
	 where	{-# INLINE fillLine4 #-}
		fillLine4 !ix
		 | ix + 4 > ixLineEnd 	= fillLine1 ix
		 | otherwise
		 = do
			let x0		= getElemFVB (ix + 0)
			let x1		= getElemFVB (ix + 1)
			let x2		= getElemFVB (ix + 2)
			let x3		= getElemFVB (ix + 3)
						
			touch x0
			touch x1
			touch x2
			touch x3
									
			VM.unsafeWrite vec (ix + 0) x0
			VM.unsafeWrite vec (ix + 1) x1
			VM.unsafeWrite vec (ix + 2) x2
			VM.unsafeWrite vec (ix + 3) x3
			fillLine4 (ix + 4)

		{-# INLINE fillLine1 #-}
		fillLine1 !ix
 	   	 | ix > ixLineEnd	= return ()
	   	 | otherwise
	   	 = do	VM.unsafeWrite vec ix (getElemFVB ix)
			fillLine1 (ix + 1)
