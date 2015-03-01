
module Data.Repa.Flow.Generic.Array.Unchunk
        (unchunk_i)
where
import Data.Repa.Flow.Generic.Base
import Data.Repa.Array                  as A
#include "repa-flow.h"


-- Unchunk --------------------------------------------------------------------
-- | Take a flow of chunks and flatten it into a flow of the individual
--   elements.
unchunk_i :: (BulkI l a, States i IO)
          => Sources i IO (Array l a)   -- ^ Chunk sources.
          -> IO (Sources i IO a)        -- ^ Element sources.

unchunk_i (Sources n pullC)
 = do   
        -- States to hold the current chunk.
        -- INVARIANT: if this holds a chunk then it is non-empty.
        rChunks  <- newRefs n Nothing

        -- States to hold the current index in each chunk.
        rIxs     <- newRefs n 0

        let pullX i eat eject
             = pullStart
             where
                -- If we already have a non-empty chunk then we can return
                -- the next element from that.
                pullStart
                 = do mchunk <- readRefs rChunks i
                      case mchunk of
                       Just chunk -> pullElem chunk
                       Nothing    -> pullSource 
                {-# INLINE pullStart #-}

                -- Try to pull a non-empty chunk from the source,
                -- and then pass on to 'pullElem' which will take the next
                -- element from it.
                pullSource
                 = pullC i eat_source eject_source
                {-# INLINE pullSource #-}

                eat_source !chunk
                 | A.length chunk == 0 
                 = pullSource

                 | otherwise         
                 = do   writeRefs rChunks i (Just chunk)
                        writeRefs rIxs    i 0
                        pullElem chunk
                {-# INLINE eat_source #-}

                eject_source        
                 = eject
                {-# INLINE eject_source #-}

                -- We've got a chunk containing some elements
                pullElem !chunk
                 = do !ix    <- readRefs rIxs i

                      _      <- if (ix + 1) >= A.length chunk
                                 -- This was the last element of the chunk.
                                 -- We need to pull a new one from the source
                                 -- the next time around.
                                 then do writeRefs rChunks i Nothing
                                         writeRefs rIxs    i 0

                                 -- There are still more elements to read
                                 -- from the current chunk.
                                 else do writeRefs rIxs    i (ix + 1)

                      let !x  = index chunk ix
                      eat x
                {-# INLINE pullElem #-}
            {-# INLINE pullX #-}

        return $ Sources n pullX
{-# INLINE_FLOW unchunk_i #-}

{-
-- | Take an argument sink for individual elements, and produce a result sink
--   for chunks.
--
--   When a chunk it pushed to the result sink then all its elements are
--   pushed to the argument sink. 
--
unchunk_o :: Monad m
          => Bulk r DIM1 e
          => Sink m e -> m (Sink m (Vector r e))

unchunk_o (Sink pushX ejectX)
 = return $ Sink push_unchunk eject_unchunk
 where  
        push_unchunk !chunk
         = loop_unchunk 0
         where  !len            = size (extent chunk)
                loop_unchunk !i
                 | i >= len     = return ()
                 | otherwise    
                 = do   pushX (chunk `index` (Z :. i))
                        loop_unchunk (i + 1)
        {-# INLINE push_unchunk #-}

        eject_unchunk
         = ejectX 
        {-# INLINE eject_unchunk #-}
{-# INLINE_FLOW unchunk_o #-}
-}
