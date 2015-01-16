
module Data.Repa.Flow.Simple.IO
        ( -- ** Sourcing
          -- * Bytes
          sourceBytes,      hSourceBytes

          -- * Records
        , sourceRecords,    hSourceRecords

          -- ** Sinking
          -- * Bytes
        , sinkBytes,        hSinkBytes)

where
import Data.Repa.Flow.Simple.Base
import System.IO
import Data.Word
import Data.Repa.Array.Foreign                  as A
import Data.Repa.Array                          as A
import qualified Data.Repa.Flow.Generic.IO      as G


-- Source Records ---------------------------------------------------------------------------------
-- | Read complete records of data from a file, using the given chunk length
--
--   The records are separated by a special terminating character, which the 
--   given predicate detects. After reading a chunk of data we seek to just after the
--   last complete record that was read, so we can continue to read more complete
--   records next time.
--
--   If we cannot find an end-of-record terminator in the chunk then apply the given
--   failure action. The records can be no longer than the chunk length. This fact
--   guards against the case where a large input file is malformed and contains no 
--   end-of-record terminators, as we won't try to read the whole file into memory.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * All chunks have the same size, except possibly the last one.
--   * The provided file handle must support seeking, else you'll get an exception.
-- 
--   The file will be closed the first time the consumer tries to pull an element
--   from the associated stream when no more are available.
--
sourceRecords 
        :: FilePath             -- ^ File paths.
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> IO (Source IO (Vector UN (Vector F Word8)))

sourceRecords path len pSep aFail
 = do   s0      <- G.sourceRecords [path] len pSep aFail
        let Just s1 = wrapI_i s0
        return s1
{-# INLINE [2] sourceRecords #-}


-- | Like `fileSourceRecords`, but taking an existing file handle.
hSourceRecords 
        :: Handle               -- ^ File handles.
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> IO (Source IO (Vector UN (Vector F Word8)))

hSourceRecords h len pSep aFail
 = do   s0      <- G.hSourceRecords [h] len pSep aFail
        let Just s1 = wrapI_i s0
        return s1
{-# INLINE [2] hSourceRecords #-}


-- Source Bytes -----------------------------------------------------------------------------------
-- | Read data from a file, using the given chunk length.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * All chunks have the same size, except possibly the last one.
--
--   The file will be closed the first time the consumer tries to pull an element
--   from the associated stream when no more are available.
--
sourceBytes 
        :: FilePath  -> Int 
        -> IO (Source IO (Vector F Word8))

sourceBytes path len
 = do   s0      <- G.sourceBytes [path] len
        let Just s1 = wrapI_i s0
        return s1
{-# INLINE [2] sourceBytes #-}


-- | Like `fileSourceBytes`, but taking existing file handles.
hSourceBytes 
        :: Handle   -> Int 
        -> IO (Source IO (Vector F Word8))

hSourceBytes h len
 = do   s0      <- G.hSourceBytes [h] len
        let Just s1 = wrapI_i s0
        return s1
{-# INLINE [2] hSourceBytes #-}



-- Sinking Bytes ----------------------------------------------------------------------------------
-- | Write chunks of data to the given files.
--
--   The file will be closed when the associated stream is ejected.
--
sinkBytes :: FilePath -> IO (Sink IO (Vector F Word8))
sinkBytes path
 = do   s0      <- G.sinkBytes [path]
        let Just s1 = wrapI_o s0
        return s1
{-# INLINE [2] sinkBytes #-}


-- | Write chunks of data to the given file handles.
hSinkBytes    :: Handle   -> IO (Sink IO (Vector F Word8))
hSinkBytes h
 = do   s0      <- G.hSinkBytes [h]
        let Just s1 = wrapI_o s0
        return s1
{-# INLINE [2] hSinkBytes #-}

