
module Data.Array.Repa.Bulk.Seq.Cursored
        (fillCursoredBlock2)
where
import Data.Array.Repa.Bulk.Elt
import GHC.Exts


-- | Fill a block in a rank-2 array, sequentially.
--
--   * Blockwise filling can be more cache-efficient than linear filling for
--     rank-2 arrays.
--
--   * Using cursor functions can help to expose inter-element indexing
--     computations to the GHC and LLVM optimisers.
--
--   * Coordinates given are of the filled edges of the block.
--
--   * The block is filled in row major order from top to bottom.
-- 
--   * We need the `Elt` constraint so that we can use its `touch` function
--     to provide an order of evaluation ammenable to the LLVM optimiser.
--     You should compile your Haskell program with @-fllvm -optlo-O3@ to
--     enable LLVM's Global Value Numbering optimisation.
--
fillCursoredBlock2
        :: Elt a
        => (Int# -> a -> IO ())
                -- ^ Update function to write into result buffer.
        -> (Int# -> Int# -> cursor)
                -- ^ Make a cursor to a particular element from an (x, y) index.
        -> (Int# -> Int# -> cursor -> cursor) 
                -- ^ Shift the cursor by an (x, y) offset.
        -> (cursor -> a) -- ^ Function to evaluate an element at the given index.
        -> Int#          -- ^ Width of the whole array.
        -> Int#          -- ^ x0 lower left corner of block to fill.
        -> Int#          -- ^ y0
        -> Int#          -- ^ w0 width of block to fill
        -> Int#          -- ^ h0 height of block to fill
        -> IO ()

fillCursoredBlock2
        write
        makeCursor shiftCursor getElem
        !imageWidth !x0 !y0 !w0 h0

 = do   fillBlock y0
 where  !x1     = x0 +# w0
        !y1     = y0 +# h0

        fillBlock !y
         | y >=# y1     = return ()
         | otherwise
         = do   fillLine4 x0
                fillBlock (y +# 1#)

         where  fillLine4 !x
                 | x +# 4# >=# x1       = fillLine1 x
                 | otherwise
                 = do   -- Compute each source cursor based on the previous one
                        -- so that the variable live ranges in the generated
                        -- code are shorter.
                        let srcCur0     = makeCursor  x  y 
                        let srcCur1     = shiftCursor 1# 0# srcCur0
                        let srcCur2     = shiftCursor 1# 0# srcCur1
                        let srcCur3     = shiftCursor 1# 0# srcCur2

                        -- Get the result value for each cursor.
                        let val0        = getElem srcCur0
                        let val1        = getElem srcCur1
                        let val2        = getElem srcCur2
                        let val3        = getElem srcCur3

                        -- Ensure that we've computed each of the result values
                        -- before we write into the array. If the backend code
                        -- generator can't tell our destination array doesn't
                        -- alias with the source then writing to it can prevent
                        -- the sharing of intermediate computations.
                        touch val0
                        touch val1
                        touch val2
                        touch val3

                        -- Compute row-major index into destination array.
                        let !dstCur0    = x +# (y *# imageWidth)
                        write  dstCur0        val0
                        write (dstCur0 +# 1#) val1
                        write (dstCur0 +# 2#) val2
                        write (dstCur0 +# 3#) val3
                        fillLine4 (x +# 4#)
                {-# INLINE fillLine4 #-}
                
                fillLine1 !x
                 | x >=# x1       = return ()
                 | otherwise
                 = do   let val0  = getElem $ makeCursor x y
                        write (x +# (y *# imageWidth)) val0
                        fillLine1 (x +# 1#)
                {-# INLINE fillLine1 #-}
        {-# INLINE fillBlock #-}

{-# INLINE [0] fillCursoredBlock2 #-}
