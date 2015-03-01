
module Data.Repa.Flow.Simple.Chunk
        ( chunk_i
        , unchunk_i,    unchunk_o)
where
import Data.Repa.Flow.Simple.Base
import Data.Repa.Array
import Data.Repa.Eval.Array
import Data.IORef
#include "repa-flow.h"



-- Unchunk ----------------------------------------------------------------------------------------
-- | Take a flow of chunks and flatten it into a flow of tge individual elements.
unchunk_i :: Bulk r DIM1 a
          => Source IO (Vector r a) -> IO (Source IO a)

unchunk_i (Source pullC)
 = do   
        -- IORef to hold the current chunk.
        -- INVARIANT: if this holds a chunk then it is non-empty.
        rmchunk  <- newIORef Nothing

        -- IORef to hold the current index into the chunk
        rix      <- newIORef 0

        let pullX eat eject
             = pullStart
             where
                -- If we already have a non-empty chunk then we can return
                -- the next element from that.
                pullStart
                 = do mchunk <- readIORef rmchunk
                      case mchunk of
                       Just chunk -> pullElem chunk
                       Nothing    -> pullSource 
                {-# INLINE pullStart #-}

                -- Try to pull a non-empty chunk from the source,
                -- and then pass on to 'pullElem' which will take the next
                -- element from it.
                pullSource
                 = pullC eat_source eject_source
                {-# INLINE pullSource #-}

                eat_source !chunk
                 | size (extent chunk) == 0 
                 = pullSource

                 | otherwise         
                 = do   writeIORef rmchunk (Just chunk)
                        writeIORef rix     0
                        pullElem chunk
                {-# INLINE eat_source #-}

                eject_source        
                 = eject
                {-# INLINE eject_source #-}

                -- We've got a chunk containing some elements
                pullElem !chunk
                 = do !ix    <- readIORef rix

                      _      <- if (ix + 1) >= size (extent chunk)
                                 -- This was the last element of the chunk.
                                 -- We need to pull a new one from the source
                                 -- the next time around.
                                 then do writeIORef rmchunk Nothing
                                         writeIORef rix     0

                                 -- There are still more elements to read
                                 -- from the current chunk.
                                 else do writeIORef rix     (ix + 1)

                      let !x  = index chunk (Z :. ix)
                      eat x
                {-# INLINE pullElem #-}
            {-# INLINE pullX #-}

        return $ Source pullX
{-# INLINE_FLOW unchunk_i #-}


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

