
module Data.Array.Repa.Flow.Internals.Chunk
        ( chunk_i
        , unchunk_i,    unchunk_o)
where
import Data.Array.Repa.Flow.Internals.Base
import Data.Array.Repa.Bulk
import Data.IORef


-- Chunk ------------------------------------------------------------------------------------------
-- | Take elements from a flow and pack them into chunks of the given
--   maximum length.
chunk_i :: Target r a
        => Int -> Source a -> IO (Source (Vector r a))

chunk_i !maxLen (Source pullX)
 = return $ Source pull
 where
  pull eat eject
   = do -- New buffer to hold elements we read from the source.
        chunk   <- unsafeNewBuffer maxLen

        -- IORef used for signalling how many elements we managed
        -- to read for this chunk.
        final   <- newIORef Nothing

        let loop_chunk !ix
                -- The chunk is already full.
                | ix >= maxLen
                =  writeIORef final (Just ix)

                |  otherwise
                =  pullX eat_chunk eject_chunk
                where   
                        -- Write the next element to the chunk.
                        eat_chunk x
                         = do   unsafeWriteBuffer chunk ix x
                                loop_chunk (ix + 1)

                        -- There are no more elements available from the soruce.
                        eject_chunk
                         -- We don't have a current chunk so we're done.     
                         | ix == 0      = writeIORef final Nothing

                         -- We've got a current chunk, so signal
                         -- that it needs to be passed on downstream.
                         | otherwise    = writeIORef final (Just ix)
            {-# INLINE loop_chunk #-}

        -- Pull as many elements as we can into a chunk.
        loop_chunk 0

        -- See what happened.
        mlen    <- readIORef final

        case mlen of
         -- We couldn't read any more elements to start a new
         -- chunk, so the source is empty.
         Nothing        
          -> eject

         -- Pass this chunk downstream.
         Just len       
          -> do chunk'  <- unsafeSliceBuffer 0 len chunk
                arr     <- unsafeFreezeBuffer (Z :. len) chunk'
                eat arr
  {-# INLINE [1] pull #-}

{-# INLINE [2] chunk_i #-}


-- Unchunk ----------------------------------------------------------------------------------------
-- | Take a flow of chunks and flatten it into a flow of tge individual elements.
unchunk_i :: Bulk r DIM1 a
          => Source (Vector r a) -> IO (Source a)

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
                {-# INLINE [1] pullElem #-}

        return $ Source pullX
{-# INLINE [2] unchunk_i #-}


-- | Take an argument sink for individual elements, and produce a result sink
--   for chunks.
--
--   When a chunk it pushed to the result sink then all its elements are
--   pushed to the argument sink. 
--
unchunk_o :: Bulk r DIM1 e
          => Sink e -> IO (Sink (Vector r e))

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
{-# INLINE [2] unchunk_o #-}

