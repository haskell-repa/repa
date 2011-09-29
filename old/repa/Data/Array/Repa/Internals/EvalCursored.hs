
{-# LANGUAGE BangPatterns, UnboxedTuples #-}
module Data.Array.Repa.Internals.EvalCursored
	( fillCursoredBlock2P
	, fillCursoredBlock2 )
where
import Data.Array.Repa.Index
import Data.Array.Repa.Internals.Elt
import Data.Array.Repa.Internals.Gang
import GHC.Base					(remInt, quotInt)
import Prelude					as P


-- Block filling ----------------------------------------------------------------------------------
-- | Fill a block in a 2D image, in parallel.
--   Coordinates given are of the filled edges of the block.
--   We divide the block into columns, and give one column to each thread.
fillCursoredBlock2P
	:: Elt a
	=> (Int -> a -> IO ())		-- ^ Update function to write into result buffer
	-> (DIM2   -> cursor)		-- ^ make a cursor to a particular element
	-> (DIM2   -> cursor -> cursor)	-- ^ shift the cursor by an offset
	-> (cursor -> a)		-- ^ fn to evaluate an element at the given index.
	-> Int			-- ^ width of whole image
	-> Int			-- ^ x0 lower left corner of block to fill
	-> Int			-- ^ y0 (low x and y value)
	-> Int			-- ^ x1 upper right corner of block to fill
	-> Int			-- ^ y1 (high x and y value, index of last elem to fill)
	-> IO ()

{-# INLINE [0] fillCursoredBlock2P #-}
fillCursoredBlock2P
	!write
	!makeCursorFCB !shiftCursorFCB !getElemFCB
	!imageWidth !x0 !y0 !x1 !y1
 = 	gangIO theGang fillBlock
 where	!threads	= gangSize theGang
	!blockWidth	= x1 - x0 + 1

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
		!x1'	= colIx (ix + 1) - 1
		!y0'	= y0
		!y1'	= y1
	   in	fillCursoredBlock2
			write
			makeCursorFCB shiftCursorFCB getElemFCB
			imageWidth x0' y0' x1' y1'


-- | Fill a block in a 2D image.
--   Coordinates given are of the filled edges of the block.
fillCursoredBlock2
	:: Elt a
	=> (Int -> a -> IO ())		-- ^ Update function to write into result buffer
	-> (DIM2   -> cursor)		-- ^ make a cursor to a particular element
	-> (DIM2   -> cursor -> cursor)	-- ^ shift the cursor by an offset
	-> (cursor -> a)		-- ^ fn to evaluate an element at the given index.
	-> Int				-- ^ width of whole image
	-> Int				-- ^ x0 lower left corner of block to fill
	-> Int				-- ^ y0 (low x and y value)
	-> Int				-- ^ x1 upper right corner of block to fill
	-> Int				-- ^ y1 (high x and y value, index of last elem to fill)
	-> IO ()

{-# INLINE [0] fillCursoredBlock2 #-}
fillCursoredBlock2
	!write
	!makeCursor !shiftCursor !getElem
	!imageWidth !x0 !y0 !x1 !y1

 = fillBlock y0

 where	{-# INLINE fillBlock #-}
	fillBlock !y
	 | y > y1	= return ()
	 | otherwise
	 = do	fillLine4 x0
		fillBlock (y + 1)

	 where	{-# INLINE fillLine4 #-}
		fillLine4 !x
 	   	 | x + 4 > x1 		= fillLine1 x
	   	 | otherwise
	   	 = do	-- Compute each source cursor based on the previous one so that
			-- the variable live ranges in the generated code are shorter.
			let srcCur0	= makeCursor  (Z :. y :. x)
			let srcCur1	= shiftCursor (Z :. 0 :. 1) srcCur0
			let srcCur2	= shiftCursor (Z :. 0 :. 1) srcCur1
			let srcCur3	= shiftCursor (Z :. 0 :. 1) srcCur2

			-- Get the result value for each cursor.
			let val0	= getElem srcCur0
			let val1	= getElem srcCur1
			let val2	= getElem srcCur2
			let val3	= getElem srcCur3

			-- Ensure that we've computed each of the result values before we
			-- write into the array. If the backend code generator can't tell
			-- our destination array doesn't alias with the source then writing
			-- to it can prevent the sharing of intermediate computations.
			touch val0
			touch val1
			touch val2
			touch val3

			-- Compute cursor into destination array.
			let !dstCur0	= x + y * imageWidth
			write (dstCur0)     val0
			write (dstCur0 + 1) val1
			write (dstCur0 + 2) val2
			write (dstCur0 + 3) val3
			fillLine4 (x + 4)

		{-# INLINE fillLine1 #-}
		fillLine1 !x
 	   	 | x > x1		= return ()
	   	 | otherwise
	   	 = do	write (x + y * imageWidth) (getElem $ makeCursor (Z :. y :. x))
			fillLine1 (x + 1)

