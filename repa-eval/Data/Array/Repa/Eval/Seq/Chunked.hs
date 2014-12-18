
module Data.Array.Repa.Eval.Seq.Chunked
        ( fillLinear
        , fillBlock2)
where
import GHC.Exts


-------------------------------------------------------------------------------
-- | Fill something sequentially.
-- 
--   * The array is filled linearly from start to finish.  
-- 
fillLinear
        :: (Int# -> a -> IO ())  -- ^ Update function to write into result buffer.
        -> (Int# -> a)           -- ^ Function to get the value at a given index.
        -> Int#                  -- ^ Number of elements to fill.
        -> IO ()

fillLinear write getElem len
 = fill 0#
 where  fill !ix
         | 1# <- ix >=# len   = return ()
         | otherwise
         = do   write ix (getElem ix)
                fill (ix +# 1#)
{-# INLINE [0] fillLinear #-}


-------------------------------------------------------------------------------
-- | Fill a block in a rank-2 array, sequentially.
--
--   * Blockwise filling can be more cache-efficient than linear filling for
--     rank-2 arrays.
--
--   * The block is filled in row major order from top to bottom.
--
fillBlock2
        :: (Int# -> a -> IO ()) -- ^ Update function to write into result buffer.
        -> (Int# -> Int# -> a)  -- ^ Function to get the value at an (x, y) index.
        -> Int#                 -- ^ Width of the whole array.
        -> Int#                 -- ^ x0 lower left corner of block to fill.
        -> Int#                 -- ^ y0
        -> Int#                 -- ^ w0 width of block to fill
        -> Int#                 -- ^ h0 height of block to fill
        -> IO ()

fillBlock2
        write getElem
        !imageWidth !x0 !y0 !w0 h0

 = do   fillBlock y0 ix0
 where  !x1     = x0 +# w0
        !y1     = y0 +# h0
        !ix0    = x0 +# (y0 *# imageWidth)

        {-# INLINE fillBlock #-}
        fillBlock !y !ix
         | 1# <- y >=# y1     = return ()
         | otherwise
         = do   fillLine1 x0 ix
                fillBlock (y +# 1#) (ix +# imageWidth)

         where  {-# INLINE fillLine1 #-}
                fillLine1 !x !ix'
                 | 1# <- x >=# x1             = return ()
                 | otherwise
                 = do   write ix' (getElem x y)
                        fillLine1 (x +# 1#) (ix' +# 1#)

{-# INLINE [0] fillBlock2 #-}
