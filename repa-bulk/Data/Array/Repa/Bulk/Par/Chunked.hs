
module Data.Array.Repa.Bulk.Par.Chunked
        ( fillChunked
        , fillChunkedIO)
where
import Data.Array.Repa.Bulk.Par.Gang
import GHC.Exts

-------------------------------------------------------------------------------
-- | Fill something in parallel.
-- 
--   * The array is split into linear chunks,
--     and each thread linearly fills one chunk.
-- 
fillChunked
        :: Gang                  -- ^ Gang to run the operation on.
        -> (Int# -> a -> IO ())  -- ^ Update function to write into result buffer.
        -> (Int# -> a)           -- ^ Function to get the value at a given index.
        -> Int#                  -- ^ Number of elements.
        -> IO ()

fillChunked gang write getElem len
 = gangIO gang
 $  \thread -> 
    let !start   = splitIx thread
        !end     = splitIx (thread +# 1#)
    in  fill start end

 where
        -- Decide now to split the work across the threads.
        -- If the length of the vector doesn't divide evenly among the threads,
        -- then the first few get an extra element.
        !threads        = gangSize gang
        !chunkLen       = len `quotInt#` threads
        !chunkLeftover  = len `remInt#`  threads

        splitIx thread
         | thread <# chunkLeftover = thread *# (chunkLen +# 1#)
         | otherwise               = thread *# chunkLen  +# chunkLeftover
        {-# INLINE splitIx #-}

        -- Evaluate the elements of a single chunk.
        fill !ix !end
         | ix >=# end           = return ()
         | otherwise
         = do   write ix (getElem ix)
                fill (ix +# 1#) end
        {-# INLINE fill #-}

{-# INLINE [0] fillChunked #-}


-------------------------------------------------------------------------------
-- | Fill something in parallel, using a separate IO action for each thread.
--
--   * The array is split into linear chunks,
--     and each thread linearly fills one chunk.
--
fillChunkedIO
        :: Gang  -- ^ Gang to run the operation on.
        -> (Int# -> a -> IO ())          
                 -- ^ Update function to write into result buffer.
        -> (Int# -> IO (Int# -> IO a))    
                 -- ^ Create a function to get the value at a given index.
                 --   The first argument is the thread number, so you can do some
                 --   per-thread initialisation.
        -> Int#  -- ^ Number of elements.
        -> IO ()

fillChunkedIO gang write mkGetElem len
 = gangIO gang
 $  \thread -> 
    let !start = splitIx thread
        !end   = splitIx (thread +# 1#)
    in fillChunk thread start end 

 where
        -- Decide now to split the work across the threads.
        -- If the length of the vector doesn't divide evenly among the threads,
        -- then the first few get an extra element.
        !threads        = gangSize gang
        !chunkLen       = len `quotInt#` threads
        !chunkLeftover  = len `remInt#`  threads

        splitIx thread
         | thread <# chunkLeftover = thread *# (chunkLen +# 1#)
         | otherwise               = thread *# chunkLen  +# chunkLeftover
        {-# INLINE splitIx #-}

        -- Given the threadId, starting and ending indices. 
        --      Make a function to get each element for this chunk
        --      and call it for every index.
        fillChunk !thread !ixStart !ixEnd
         = do   getElem <- mkGetElem thread
                fill getElem ixStart ixEnd
        {-# INLINE fillChunk #-}
                
        -- Call the provided getElem function for every element
        --      in a chunk, and feed the result to the write function.
        fill !getElem !ix0 !end
         = go ix0 
         where  go !ix
                 | ix >=# end   = return ()
                 | otherwise
                 = do   x       <- getElem ix
                        write ix x
                        go (ix +# 1#)
        {-# INLINE fill #-}

{-# INLINE [0] fillChunkedIO #-}
