{-# LANGUAGE BangPatterns, ExistentialQuantification, MagicHash, ScopedTypeVariables #-}

module ChainChunk (test) where
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector.Unboxed.Mutable    as UM
import Control.Monad.ST
import System.IO.Unsafe
import GHC.Exts

data Step a s
        = Yield  a s
        | Update s 

data Chain a
        = forall s
        . Chain Int#                    -- Total length of stream.
                Int#                    -- Number of chunks.
                (Int# -> Chunk s a)     -- Chunk for each thread.

data Chunk s a
        = Chunk Int#                    -- Start position of this chunk.
                Int#                    -- End   position of this chunk.
                (Int -> s -> Step a s)  -- Step function
                s                       -- Initial state for this chunk.


-------------------------------------------------------------------------------
-- | Convert a vector to a chain.
vchain  :: U.Unbox a 
        => Int                          -- ^ Number of chunks in the chain.
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
         = Yield (vec `U.unsafeIndex` ix) (ix + 1)
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

         where  fillChunk' ix s
                 | ix >=# end
                 = return ()

                 | otherwise
                 = case mkStep (I# c) s of
                        Yield x s'
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
                        Yield x s'      -> Yield (f x) s'
                        Update s'       -> Update s'
                {-# INLINE [0] mkStep' #-}
           in   Chunk start end mkStep' s0
        {-# INLINE [1] mkChunk' #-}

{-# INLINE [1] cmap #-}


-------------------------------------------------------------------------------

test :: U.Vector Int -> U.Vector Int
test vec
        = vunchain $ cmap (+ 12345) $ vchain 4 vec

