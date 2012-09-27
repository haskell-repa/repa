{-# LANGUAGE BangPatterns, ExistentialQuantification, MagicHash, ScopedTypeVariables #-}

module ChainChunk (test) where
import Data.Array.Parallel.Unlifted.Parallel.UPSegd             (UPSegd)
import qualified Data.Vector.Unboxed                            as U
import qualified Data.Vector.Unboxed.Mutable                    as UM
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd   as UPSegd
import qualified Data.Array.Parallel.Unlifted.Distributed.Primitive.DT  as D
import Control.Monad.ST
import System.IO.Unsafe
import GHC.Exts


data Step s a
        = Yield  s a
        | Update s 


data Chunk s a
        = Chunk Int#                    -- Start position of this chunk.
                Int#                    -- End   position of this chunk.
                (Int -> s -> Step s a)  -- Step function
                s                       -- Initial state for this chunk.

data Chain a
        = forall s
        . Chain Int#                    -- Total length of stream.
                Int#                    -- Number of chunks.
                (Int# -> Chunk s a)     -- Chunk for each thread.


-------------------------------------------------------------------------------
-- | Convert a vector to a chain.
vchain  :: U.Unbox a 
        => Int
        -> U.Vector a                   -- ^ Vector to convert.
        -> Chain a

vchain (I# chunks) !vec
 = Chain len chunks mkChunk
 where  
        I# len     = U.length vec

        mkChunk c
         = let  start   = nstart chunks len c
                end     = nstart chunks len (c +# 1#)
           in   vchunk vec start end
        {-# INLINE [1] mkChunk #-}
{-# INLINE [1] vchain #-}


-- | Create a chunk for part of this vector.
vchunk  :: U.Unbox a 
        => U.Vector a 
        -> Int#                         -- ^ Start index.
        -> Int#                         -- ^ End index.
        -> Chunk Int a

vchunk vec start end
 = Chunk start end mkStep (I# start)
 where  
        mkStep n ix
         = Yield (ix + 1) (vec `U.unsafeIndex` ix)
{-# INLINE [1] vchunk #-}


-- Get the starting point for a chunk.
nstart  :: Int# -> Int# -> Int# -> Int#
nstart chunks len c
 = let  chunkLen        = len `quotInt#` chunks
        chunkLeftover   = len `remInt#`  chunks

        getStart c'
         | c' <# chunkLeftover = c' *# (chunkLen +# 1#)
         | otherwise           = c' *# chunkLen  +# chunkLeftover
        {-# NOINLINE getStart #-}

  in    getStart c
{-# NOINLINE nstart #-}


-------------------------------------------------------------------------------
-- | Convert a chain back to a vector.
vunchain :: U.Unbox a => Chain a -> U.Vector a
vunchain (Chain len chunks mkChunk)
 = runST
 $ do   mvec    <- UM.unsafeNew (I# len)
        fillChunks (UM.unsafeWrite mvec)
        U.unsafeFreeze mvec

 where  fillChunks write 
         = fillChunks' 0#
         where  
                fillChunks' c
                 | c >=# chunks 
                 = return ()

                 | otherwise
                 = do   fillChunk  write c (mkChunk c)
                        fillChunks' (c +# 1#)
        {-# INLINE [0] fillChunks #-}

        fillChunk write c (Chunk start end mkStep s0)
         = fillChunk' start s0
         where  
                fillChunk' ix s
                 | ix >=# end
                 = return ()

                 | otherwise
                 = case mkStep (I# c) s of
                        Yield  s' x
                         -> do  write (I# ix) x
                                fillChunk' (ix +# 1#) s'

                        Update s'
                         ->     fillChunk' ix s'
        {-# INLINE [0] fillChunk #-}
{-# INLINE [1] vunchain #-}


-------------------------------------------------------------------------------
-- | Apply a function to every element of a chain.
cmap :: (a -> b) -> Chain a -> Chain b
cmap f (Chain len chunks mkChunk)
 = Chain len chunks mkChunk'

 where  mkChunk' c
         | Chunk start end mkStep s0    <- mkChunk c
         = let  mkStep' ix s
                 = case mkStep ix s of
                        Yield  s' x     -> Yield s' (f x) 
                        Update s'       -> Update s'
                {-# INLINE [0] mkStep' #-}
           in   Chunk start end mkStep' s0
        {-# INLINE [1] mkChunk' #-}
{-# INLINE [1] cmap #-}


-------------------------------------------------------------------------------
vreplicates
        :: U.Unbox a
        => UPSegd
        -> U.Vector a
        -> Chain a

vreplicates upsegd vec
 = let  
        -- Total number of elements to produce.
        !(I# len)       = UPSegd.takeElements  upsegd

        -- Get the distributed segment descriptor.
        -- This tells us what elements each thread should work on.
        !dists          = UPSegd.takeDistributed upsegd

        -- Number of chunks in the distributed segment descriptor.
        !(I# chunks)    = D.sizeD dists

        -- Function to distribute work over gang,
        -- 'c' is thread number
        mkChunk !c
         = let  -- TODO: Uh oh, this is assuming segd.takeDistributed works
                --              the same as nstart...
                --              We should use the offsets from takeDistributed instead.
                !start  = nstart chunks len c
                !end    = nstart chunks len (c +# 1#)
        
                -- Get current chunk of segment.
                -- Segment offset describes where segment 0 in segd corrseponds in upsegd
                ((segd, seg_off), _)    = D.indexD "replicates" dists (I# c)
        
                -- State: current segment -1 and remaining 0,
                -- so first step will calculate real values
                !state  = (segd, seg_off, -1, 0)

           in   Chunk start end step state
        {-# INLINE [2] mkChunk #-}


        -- Stepper is called until the entire buffer is filled.
        step _ (!segd, !seg_off, !seg_cur, !remain)
         -- Go to next segment.
         -- Don't need to handle end case because caller stops looping      
         = if remain == 0
            then
             let !seg_cur'       = seg_cur + 1

                 -- Find out how many copies we need to fill.
                 (remain', _)    = USegd.getSeg segd seg_cur'

                 -- Don't return a value, just update state .
             in  Update (segd, seg_off, seg_cur', remain')

            -- Return a value and decrement the count remaining   
            else Yield  (segd, seg_off, seg_cur, remain - 1)
                        (vec `U.unsafeIndex` (seg_off + seg_cur))
        {-# INLINE [1] step #-}


  in    Chain len chunks mkChunk
{-# INLINE vreplicates #-}


-------------------------------------------------------------------------------
test :: UPSegd -> U.Vector Int -> U.Vector Int
test !upsegd !vec
        = vunchain $ cmap (+ 12345) $ vreplicates upsegd vec

