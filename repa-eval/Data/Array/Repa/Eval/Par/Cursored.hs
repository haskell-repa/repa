
module Data.Array.Repa.Eval.Par.Cursored
        ( fillBlock2
        , fillCursoredBlock2)
where
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Eval.Gang
import qualified Data.Array.Repa.Eval.Seq.Cursored      as Seq
import GHC.Exts


-- Non-cursored interface -----------------------------------------------------
-- | Fill a block in a rank-2 array in parallel.
--
--   * Blockwise filling can be more cache-efficient than linear filling for
--     rank-2 arrays.
--
--   * Coordinates given are of the filled edges of the block.
-- 
--   * We divide the block into columns, and give one column to each thread.
-- 
--   * Each column is filled in row major order from top to bottom.
--
fillBlock2 
        :: Elt a
        => Gang
        -> (Int# -> a -> IO ()) 
                        -- ^ Update function to write into result buffer.
        -> (Int# -> Int# -> a)  
                        -- ^ Function to evaluate the element at an (x, y) index.
        -> Int#         -- ^ Width of the whole array.
        -> Int#         -- ^ x0 lower left corner of block to fill
        -> Int#         -- ^ y0 
        -> Int#         -- ^ w0 width of block to fill.
        -> Int#         -- ^ h0 height of block to fill.
        -> IO ()

fillBlock2 gang write getElem !imageWidth !x0 !y0 !w0 h0
 = fillCursoredBlock2
        gang write
        makeCursor shiftCursor loadCursor
        imageWidth x0 y0 w0 h0

 where  makeCursor x y
                = DIM2 x y
        {-# INLINE makeCursor #-}

        shiftCursor x' y' (DIM2 x y) 
                = DIM2 (x +# x') (y +# y')
        {-# INLINE shiftCursor #-}

        loadCursor (DIM2 x y)
                = getElem x y
        {-# INLINE loadCursor #-}

{-# INLINE [0] fillBlock2 #-}

data DIM2 
        = DIM2 Int# Int#


-- Block filling --------------------------------------------------------------
-- | Fill a block in a rank-2 array in parallel.
-- 
--   * Blockwise filling can be more cache-efficient than linear filling for
--     rank-2 arrays.
--
--   * Using cursor functions can help to expose inter-element indexing
--     computations to the GHC and LLVM optimisers.
--
--   * Coordinates given are of the filled edges of the block.
--
--   * We divide the block into columns, and give one column to each thread.
-- 
--   * We need the `Elt` constraint so that we can use its `touch` function
--     to provide an order of evaluation ammenable to the LLVM optimiser.
--     You should compile your Haskell program with @-fllvm -optlo-O3@ to
--     enable LLVM's Global Value Numbering optimisation.
--
fillCursoredBlock2
        :: Elt a
        => Gang -- ^ Gang to run the operation on.
        -> (Int# -> a -> IO ())          
                -- ^ Update function to write into result buffer.
        -> (Int# -> Int# -> cursor)           
                -- ^ Make a cursor from an (x, y) index.
        -> (Int# -> Int# -> cursor -> cursor) 
                -- ^ Shift the cursor by an (x, y) offset.
        -> (cursor -> a) -- ^ Function to evaluate the element at an index.
        -> Int#          -- ^ Width of the whole array.
        -> Int#          -- ^ x0 lower left corner of block to fill
        -> Int#          -- ^ y0
        -> Int#          -- ^ w0 width of block to fill
        -> Int#          -- ^ h0 height of block to fill
        -> IO ()

fillCursoredBlock2
        gang write
        makeCursorFCB shiftCursorFCB getElemFCB
        !imageWidth !x0 !y0 !w0 !h0
 =      gangIO gang fillBlock
 where  
        !threads        = gangSize gang

        -- All columns have at least this many pixels.
        !colChunkLen   = w0 `quotInt#` threads

        -- Extra pixels that we have to divide between some of the threads.
        !colChunkSlack = w0 `remInt#` threads

        -- Get the starting pixel of a column in the image.
        colIx !ix
         | 1# <- ix <# colChunkSlack = x0 +# (ix *# (colChunkLen +# 1#))
         | otherwise                 = x0 +# (ix *# colChunkLen) +# colChunkSlack
        {-# INLINE colIx #-}

        -- Give one column to each thread
        fillBlock :: Int# -> IO ()
        fillBlock !ix
         = let  !x0'      = colIx ix
                !w0'      = colIx (ix +# 1#) -# x0'
                !y0'      = y0
                !h0'      = h0
           in   Seq.fillCursoredBlock2
                        write
                        makeCursorFCB shiftCursorFCB getElemFCB
                        imageWidth x0' y0' w0' h0'
        {-# INLINE fillBlock #-}

{-# INLINE [0] fillCursoredBlock2 #-}

