
module Data.Repa.Flow.Generic.Array.Chunk
        (chunk_i)
where
import Data.Repa.Flow.Generic.Base
import Data.Repa.Array.Generic                  as A
import Data.Repa.Array.Generic.Index            as A
import Data.Repa.Array.Generic.Target           as A
#include "repa-flow.h"


-- | Take elements from a flow and pack them into chunks of the given
--   maximum length.
chunk_i :: (Target lDst a, Index lDst ~ Int, States i IO)
        => Name lDst                            -- ^ Layout for result chunks.
        -> Int                                  -- ^ Maximum chunk length.
        -> Sources i IO a                       -- ^ Element sources.
        -> IO (Sources i IO (Array lDst a))     -- ^ Chunk sources.

chunk_i nDst !maxLen (Sources n pullX)
 = do
        -- Refs for signalling how many elements we managed to read for
        -- each chunk.
        final  <- newRefs n Nothing

        let pull_chunk i eat eject
             = do 
                -- New buffer to hold elements we read from the source.
                chunk   <- unsafeNewBuffer (A.create nDst maxLen)
               
                let loop_chunk !ix
                        -- The chunk is already full.
                        | ix >= maxLen
                        = writeRefs final i (Just ix)
        
                        | otherwise
                        = pullX i eat_chunk eject_chunk
                        where   
                                -- Write the next element to the chunk.
                                eat_chunk x
                                 = do   unsafeWriteBuffer chunk ix x
                                        loop_chunk (ix + 1)
        
                                -- There are no more elements available from the soruce.
                                eject_chunk
                                 -- We don't have a current chunk so we're done.     
                                 | ix == 0      = writeRefs final i Nothing
        
                                 -- We've got a current chunk, so signal
                                 -- that it needs to be passed on downstream.
                                 | otherwise    = writeRefs final i (Just ix)
                    {-# INLINE loop_chunk #-}
        
                -- Pull as many elements as we can into a chunk.
                loop_chunk 0
        
                -- See what happened.
                mlen    <- readRefs final i
        
                case mlen of
                 -- We couldn't read any more elements to start a new
                 -- chunk, so the source is empty.
                 Nothing        -> eject
        
                 -- Pass this chunk downstream.
                 Just len       
                   -> do chunk'  <- unsafeSliceBuffer  0 len chunk
                         arr     <- unsafeFreezeBuffer chunk'
                         eat arr
            {-# INLINE pull_chunk #-}

        return $ Sources n pull_chunk
{-# INLINE_FLOW chunk_i #-}

