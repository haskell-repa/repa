
{-# LANGUAGE BangPatterns #-}
module Data.Array.Repa.Internals.EvalBlockwise2
	(fillVectorBlock2)
where
import Data.Array.Repa.Internals.Gang
import Data.Vector.Unboxed			as V
import Data.Vector.Unboxed.Mutable		as VM
import System.IO.Unsafe
import GHC.Base					(remInt, quotInt)
import Prelude					as P


-- | Fill a block in a 2D image.
--   Coordinates given are of the filled edges of the block.
fillVectorBlock2
	:: Unbox a
	=> IOVector a		-- ^ vector to write elements into.
	-> (Int -> Int -> a)	-- ^ fn to evaluate an element at the given index.
	-> Int			-- ^ width of whole image
	-> Int			-- ^ x0 lower left corner of block to fill 
	-> Int			-- ^ y0 (low x and y value)
	-> Int			-- ^ x1 upper right corner of block to fill
	-> Int			-- ^ y1 (high x and y value)
	-> IO ()

{-# INLINE fillVectorBlock2 #-}
fillVectorBlock2 !vec !getElemFVB !imageWidth !x0 !y0 !x1 !y1
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
	   	 = do	let !ix0	= x + 0 + y * imageWidth
			let !ix1	= ix0 + 1
			let !ix2	= ix0 + 2
			let !ix3	= ix0 + 3
				
			VM.unsafeWrite vec ix0 (getElemFVB (x + 0) y)
			VM.unsafeWrite vec ix1 (getElemFVB (x + 1) y)
			VM.unsafeWrite vec ix2 (getElemFVB (x + 2) y)
			VM.unsafeWrite vec ix3 (getElemFVB (x + 3) y)
			fillLine4 (x + 4)
		
		{-# INLINE fillLine1 #-}
		fillLine1 !x
 	   	 | x > x1	= return ()
	   	 | otherwise
	   	 = do	VM.unsafeWrite vec (x + y * imageWidth) (getElemFVB x y)
			fillLine1 (x + 1)

{-
fillVectorBlock3x1 
	:: Unbox a
	=> IOVector a		-- ^ vector to write elements into.
	-> (Int -> a)		-- ^ fn to get the coefficient
	-> Int			-- ^ width of whole image
	-> Int			-- ^ x0 lower left corner of block to fill 
	-> Int			-- ^ y0 (low x and y value)
	-> Int			-- ^ x1 upper right corner of block to fill
	-> Int			-- ^ y1 (high x and y value)
	-> IO ()
 -}