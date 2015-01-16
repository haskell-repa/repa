
-- | Read and write files.
module Data.Repa.Flow.IO
        ( -- * Sourcing
          G.fromFiles
        , sourceRecords
        , sourceLines
        , sourceBytes

          -- * Sinking
        , G.toFiles
        , sinkBytes)
where
import Data.Repa.Flow
import Data.Repa.Eval.Array                     as A
import Data.Repa.Array                          as A hiding (fromList, fromLists)
import qualified Data.Repa.Flow.Generic         as G hiding (next)
import System.IO
import Data.Word
import Data.Char


-- Source Records -------------------------------------------------------------
-- | Read complete records of data form a file, into chunks of the given length.
--   We read as many complete records as will fit into each chunk.
--
--   The records are separated by a special terminating character, which the 
--   given predicate detects. After reading a chunk of data we seek the file to 
--   just after the last complete record that was read, so we can continue to
--   read more complete records next time. 
--
--   If we cannot fit at least one complete record in the chunk then perform
--   the given failure action. Limiting the chunk length guards against the
--   case where a large input file is malformed, as we won't try to read the
--   whole file into memory.
-- 
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * The provided file handle must support seeking, else you'll get an
--     exception.
--   * Each file is closed the first time the consumer tries to pull a
--     record from the associated stream when no more are available.
--
sourceRecords 
        :: Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a
                                --   whole record.
        -> [Handle]             -- ^ File handles.
        -> IO (Sources UN (Vector F Word8))
sourceRecords = G.sourceRecords
{-# INLINE sourceRecords #-}


-- Source Lines ---------------------------------------------------------------
-- | Read complete lines of data from a text file, using the given chunk length.
--
--   * The trailing new-line characters are discarded.
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * The provided file handle must support seeking, else you'll get an
--     exception.
--   * Each file is closed the first time the consumer tries to pull a line
--     from the associated stream when no more are available.
--
sourceLines
        :: Int                  -- ^ Size of chunk to read in bytes.
        -> IO ()                -- ^ Action to perform if we can't get a
                                --   whole record.
        -> [Handle]             -- ^ File handles.
        -> IO (Sources UN (Vector F Char))
sourceLines nChunk fails hs
 =   mapChunks_i chopChunk
 =<< G.sourceRecords nChunk isNewLine fails hs
 where
        isNewLine   :: Word8 -> Bool
        isNewLine x =  x == nl
        {-# INLINE isNewLine #-}
  
        chopChunk chunk
         = A.mapElems (A.computeS_ . A.map (chr . fromIntegral)) 
         $ A.trimEnds (== nl) chunk
        {-# INLINE chopChunk #-}

        nl :: Word8
        !nl = fromIntegral $ ord '\n'
{-# INLINE sourceLines #-}


-- Source Bytes ---------------------------------------------------------------
-- | Read data from some files, using the given chunk length.
sourceBytes :: Int -> [Handle] -> IO (Sources F Word8)
sourceBytes = G.sourceBytes
{-# INLINE sourceBytes #-}


-- Sink Bytes -----------------------------------------------------------------
-- | Write bytes to the given files.
sinkBytes :: [Handle] -> IO (Sinks F Word8)
sinkBytes = G.sinkBytes
{-# INLINE sinkBytes #-}

