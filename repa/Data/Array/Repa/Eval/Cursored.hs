{-# LANGUAGE MagicHash #-}
-- | Evaluate an array by dividing it into rectangular blocks and filling
--   each block in parallel.
module Data.Array.Repa.Eval.Cursored
        ( fillBlock2P
        , fillCursoredBlock2P
        , fillCursoredBlock2S )
where
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Eval.Gang
import GHC.Base


-- Non-cursored interface -----------------------------------------------------
-- | Fill a block in a rank-2 array in parallel.
--
--   * Blockwise filling can be more cache-efficient than linear filling for
--    rank-2 arrays.
--
--   * Coordinates given are of the filled edges of the block.
-- 
--   * We divide the block into columns, and give one column to each thread.
-- 
--   * Each column is filled in row major order from top to bottom.
--
fillBlock2P 
        :: Elt a
        => (Int -> a -> IO ())  -- ^ Update function to write into result buffer.
        -> (DIM2 -> a)          -- ^ Function to evaluate the element at an index.
        -> Int#                 -- ^ Width of the whole array.
        -> Int#                 -- ^ x0 lower left corner of block to fill
        -> Int#                 -- ^ y0 
        -> Int#                 -- ^ w0 width of block to fill.
        -> Int#                 -- ^ h0 height of block to fill.
        -> IO ()

{-# INLINE [0] fillBlock2P #-}
fillBlock2P write getElem !imageWidth !x0 !y0 !w0 h0
 = fillCursoredBlock2P 
        write id addDim getElem 
        imageWidth x0 y0 w0 h0

{-
-- | Fill a block in a rank-2 array sequentially.
--
--   * Blockwise filling can be more cache-efficient than linear filling for
--    rank-2 arrays.
--
--   * Coordinates given are of the filled edges of the block.
-- 
--   * The block is filled in row major order from top to bottom.
--
fillBlock2S
        :: Elt a
        => (Int -> a -> IO ())  -- ^ Update function to write into result buffer.
        -> (DIM2 -> a)          -- ^ Function to evaluate the element at an index.
        -> Int#                 -- ^ Width of the whole array.
        -> Int#                 -- ^ x0 lower left corner of block to fill
        -> Int#                 -- ^ y0
        -> Int#                 -- ^ w0 width of block to fill
        -> Int#                 -- ^ h0 height of block to filll
        -> IO ()

{-# INLINE [0] fillBlock2S #-}
fillBlock2S write getElem !imageWidth !x0 !y0 !w0 !h0
 = fillCursoredBlock2S
        write id addDim getElem 
        imageWidth x0 y0 w0 h0
-}

-- Block filling ----------------------------------------------------------------------------------
-- | Fill a block in a rank-2 array in parallel.
-- 
--   * Blockwise filling can be more cache-efficient than linear filling for rank-2 arrays.
--
--   * Using cursor functions can help to expose inter-element indexing computations to
--     the GHC and LLVM optimisers.
--
--   * Coordinates given are of the filled edges of the block.
-- 
--   * We divide the block into columns, and give one column to each thread.
-- 
--   * Each column is filled in row major order from top to bottom.
--
fillCursoredBlock2P
        :: Elt a
        => (Int -> a -> IO ())          -- ^ Update function to write into result buffer.
        -> (DIM2   -> cursor)           -- ^ Make a cursor to a particular element.
        -> (DIM2   -> cursor -> cursor) -- ^ Shift the cursor by an offset.
        -> (cursor -> a)                -- ^ Function to evaluate the element at an index.
        -> Int#                         -- ^ Width of the whole array.
        -> Int#                         -- ^ x0 lower left corner of block to fill
        -> Int#                         -- ^ y0
        -> Int#                         -- ^ w0 width of block to fill
        -> Int#                         -- ^ h0 height of block to fill
        -> IO ()

{-# INLINE [0] fillCursoredBlock2P #-}
fillCursoredBlock2P
        write
        makeCursorFCB shiftCursorFCB getElemFCB
        !imageWidth !x0 !y0 !w0 !h0
 =      gangIO theGang fillBlock
 where  
        !(I# threads)  = gangSize theGang

        -- All columns have at least this many pixels.
        !colChunkLen   = w0 `quotInt#` threads

        -- Extra pixels that we have to divide between some of the threads.
        !colChunkSlack = w0 `remInt#` threads

        -- Get the starting pixel of a column in the image.
        {-# INLINE colIx #-}
        colIx !ix
         | 1# <- ix <# colChunkSlack = x0 +# (ix *# (colChunkLen +# 1#))
         | otherwise                 = x0 +# (ix *# colChunkLen) +# colChunkSlack

        -- Give one column to each thread
        {-# INLINE fillBlock #-}
        fillBlock :: Int -> IO ()
        fillBlock !(I# ix)
         = let  !x0'      = colIx ix
                !w0'      = colIx (ix +# 1#) -# x0'
                !y0'      = y0
                !h0'      = h0
           in   fillCursoredBlock2S
                        write
                        makeCursorFCB shiftCursorFCB getElemFCB
                        imageWidth x0' y0' w0' h0'


-- | Fill a block in a rank-2 array, sequentially.
--
--   * Blockwise filling can be more cache-efficient than linear filling for rank-2 arrays.
--
--   * Using cursor functions can help to expose inter-element indexing computations to
--     the GHC and LLVM optimisers.
--
--   * Coordinates given are of the filled edges of the block.
--
--   * The block is filled in row major order from top to bottom.
--
fillCursoredBlock2S
        :: Elt a
        => (Int -> a -> IO ())          -- ^ Update function to write into result buffer.
        -> (DIM2   -> cursor)           -- ^ Make a cursor to a particular element.
        -> (DIM2   -> cursor -> cursor) -- ^ Shift the cursor by an offset.
        -> (cursor -> a)                -- ^ Function to evaluate an element at the given index.
        -> Int#                         -- ^ Width of the whole array.
        -> Int#                         -- ^ x0 lower left corner of block to fill.
        -> Int#                         -- ^ y0
        -> Int#                         -- ^ w0 width of block to fill
        -> Int#                         -- ^ h0 height of block to fill
        -> IO ()

{-# INLINE [0] fillCursoredBlock2S #-}
fillCursoredBlock2S
        write
        makeCursor shiftCursor getElem
        !imageWidth !x0 !y0 !w0 h0

 = do   fillBlock y0
 where  !x1     = x0 +# w0
        !y1     = y0 +# h0

        {-# INLINE fillBlock #-}
        fillBlock !y
         | 1# <- y >=# y1      = return ()
         | otherwise
         = do   fillLine4 x0
                fillBlock (y +# 1#)

         where  {-# INLINE fillLine4 #-}
                fillLine4 !x
                 | 1# <- x +# 4# >=# x1  = fillLine1 x
                 | otherwise
                 = do   -- Compute each source cursor based on the previous one so that
                        -- the variable live ranges in the generated code are shorter.
                        let srcCur0     = makeCursor  (Z :. (I# y) :. (I# x))
                        let srcCur1     = shiftCursor (Z :. 0 :. 1) srcCur0
                        let srcCur2     = shiftCursor (Z :. 0 :. 1) srcCur1
                        let srcCur3     = shiftCursor (Z :. 0 :. 1) srcCur2

                        -- Get the result value for each cursor.
                        let val0        = getElem srcCur0
                        let val1        = getElem srcCur1
                        let val2        = getElem srcCur2
                        let val3        = getElem srcCur3

                        -- Ensure that we've computed each of the result values before we
                        -- write into the array. If the backend code generator can't tell
                        -- our destination array doesn't alias with the source then writing
                        -- to it can prevent the sharing of intermediate computations.
                        touch val0
                        touch val1
                        touch val2
                        touch val3

                        -- Compute cursor into destination array.
                        let !dstCur0    = x +# (y *# imageWidth)
                        write (I# dstCur0)         val0
                        write (I# (dstCur0 +# 1#)) val1
                        write (I# (dstCur0 +# 2#)) val2
                        write (I# (dstCur0 +# 3#)) val3
                        fillLine4 (x +# 4#)

                {-# INLINE fillLine1 #-}
                fillLine1 !x
                 | 1# <- x >=# x1 = return ()
                 | otherwise
                 = do   let val0  = (getElem $ makeCursor (Z :. (I# y) :. (I# x)))
                        write (I# (x +# (y *# imageWidth))) val0
                        fillLine1 (x +# 1#)

