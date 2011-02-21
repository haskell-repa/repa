
{-# LANGUAGE BangPatterns, UnboxedTuples #-}
module Data.Array.Repa.Internals.EvalCursored
	( fillCursoredBlock2P
	, fillCursoredBlock2 )
where
import Data.Array.Repa.Index
import Data.Array.Repa.Internals.Gang
import Data.Array.Repa.Internals.Elt
import Data.Vector.Unboxed.Mutable		as VM
import GHC.Base					(remInt, quotInt)
import Prelude					as P


-- Block filling ----------------------------------------------------------------------------------
-- | Fill a block in a 2D image, in parallel.
--   Coordinates given are of the filled edges of the block.
--   We divide the block into columns, and give one column to each thread.
fillCursoredBlock2P
	:: Elt a
	=> IOVector a		-- ^ vector to write elements into
	-> (DIM2   -> cursor)		-- ^ make a cursor to a particular element
	-> (DIM2   -> cursor -> cursor)	-- ^ shift the cursor by an offset
	-> (cursor -> a)		-- ^ fn to evaluate an element at the given index.
	-> Int			-- ^ width of whole image
	-> Int			-- ^ x0 lower left corner of block to fill
	-> Int			-- ^ y0 (low x and y value)
	-> Int			-- ^ x1 upper right corner of block to fill
	-> Int			-- ^ y1 (high x and y value)
	-> IO ()

{-# INLINE fillCursoredBlock2P #-}
fillCursoredBlock2P 
	!vec
	!makeCursorFCB !shiftCursorFCB !getElemFCB
	!imageWidth !x0 !y0 !x1 !y1
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
	   in	fillCursoredBlock2 
			vec 
			makeCursorFCB shiftCursorFCB getElemFCB
			imageWidth x0' y0' x1' y1'


-- | Fill a block in a 2D image.
--   Coordinates given are of the filled edges of the block.
fillCursoredBlock2
	:: Elt a
	=> IOVector a			-- ^ vector to write elements into.
	-> (DIM2   -> cursor)		-- ^ make a cursor to a particular element
	-> (DIM2   -> cursor -> cursor)	-- ^ shift the cursor by an offset
	-> (cursor -> a)		-- ^ fn to evaluate an element at the given index.
	-> Int				-- ^ width of whole image
	-> Int				-- ^ x0 lower left corner of block to fill 
	-> Int				-- ^ y0 (low x and y value)
	-> Int				-- ^ x1 upper right corner of block to fill
	-> Int				-- ^ y1 (high x and y value)
	-> IO ()

{-# INLINE fillCursoredBlock2 #-}
fillCursoredBlock2 
	!vec 
	!makeCursorFCB !shiftCursorFCB !getElemFCB
	!imageWidth !x0 !y0 !x1 !y1
 = fillBlock y0
 where	
	
	{-# INLINE fillBlock #-}
	fillBlock y
	 | y > y1	= return ()
	 | otherwise
	 = do	fillLine4 x0
		fillBlock (y + 1)
	
	 where	{-# INLINE fillLine4 #-}
		fillLine4 !x
 	   	 | x + 4 > x1	= fillLine1 x
	   	 | otherwise
	   	 = do	let c0		= makeCursorFCB  (Z :. y :. x)
			let c1		= shiftCursorFCB (Z :. 0 :. 1) c0
			let c2		= shiftCursorFCB (Z :. 0 :. 1) c1
			let c3		= shiftCursorFCB (Z :. 0 :. 1) c2

			let d0		= getElemFCB c0
			let d1		= getElemFCB c1
			let d2		= getElemFCB c2
			let d3		= getElemFCB c3
			
			touch d0
			touch d1
			touch d2
			touch d3
				
			let !ix0	= x + 0 + y * imageWidth
			let !ix1	= ix0 + 1
			let !ix2	= ix0 + 2
			let !ix3	= ix0 + 3
									
			VM.unsafeWrite vec ix0 d0
			VM.unsafeWrite vec ix1 d1
			VM.unsafeWrite vec ix2 d2 
			VM.unsafeWrite vec ix3 d3
			fillLine4 (x + 4) 
		
		{-# INLINE fillLine1 #-}
		fillLine1 !x
 	   	 | x > x1	= return ()
	   	 | otherwise
	   	 = do	VM.unsafeWrite vec (x + y * imageWidth) (getElemFCB $ makeCursorFCB (Z :. y :. x))
			fillLine1 (x + 1)


