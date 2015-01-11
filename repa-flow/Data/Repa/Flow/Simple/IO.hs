
module Data.Repa.Flow.Simple.IO
        ( -- * Sourcing Bytes
          fileSourceBytes,      hSourceBytes

          -- * Sourcing Records
        , fileSourceRecords,    hSourceRecords

          -- * Sinking Bytes
        , fileSinkBytes,        hSinkBytes)

where
import Data.Repa.Flow.Simple.Base
import System.IO
import Data.Word
import Data.Repa.Array.Foreign                  as A
import Data.Repa.Array                          as A
import qualified Data.Repa.Flow.Generic.IO      as G


-- Source Bytes -----------------------------------------------------------------------------------
-- | Read data from a file, using the given chunk length.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * All chunks have the same size, except possibly the last one.
--
--   The file will be closed the first time the consumer tries to pull an element
--   from the associated stream when no more are available.
--
fileSourceBytes 
        :: FilePath  -> Int 
        -> IO (Source IO (Vector F Word8))

fileSourceBytes path len
 = do   s0      <- G.fileSourcesBytes [path] len
        let Just s1 = wrapI_i s0
        return s1
{-# INLINE [2] fileSourceBytes #-}


-- | Like `fileSourceBytes`, but taking existing file handles.
hSourceBytes 
        :: Handle   -> Int 
        -> IO (Source IO (Vector F Word8))

hSourceBytes h len
 = do   s0      <- G.hSourcesBytes [h] len
        let Just s1 = wrapI_i s0
        return s1
{-# INLINE [2] hSourceBytes #-}


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
fileSourceRecords 
        :: FilePath             -- ^ File paths.
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> IO (Source IO (Vector UN (Vector F Word8)))

fileSourceRecords path len pSep aFail
 = do   s0      <- G.fileSourcesRecords [path] len pSep aFail
        let Just s1 = wrapI_i s0
        return s1
{-# INLINE [2] fileSourceRecords #-}


-- | Like `fileSourceRecords`, but taking an existing file handle.
hSourceRecords 
        :: Handle               -- ^ File handles.
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> IO (Source IO (Vector UN (Vector F Word8)))

hSourceRecords h len pSep aFail
 = do   s0      <- G.hSourcesRecords [h] len pSep aFail
        let Just s1 = wrapI_i s0
        return s1
{-# INLINE [2] hSourceRecords #-}


-- Sinking Bytes ----------------------------------------------------------------------------------
-- | Write chunks of data to the given files.
--
--   The file will be closed when the associated stream is ejected.
--
fileSinkBytes
        :: FilePath 
        -> IO (Sink IO (Vector F Word8))

fileSinkBytes path
 = do   s0      <- G.fileSinksBytes [path]
        let Just s1 = wrapI_o s0
        return s1
{-# INLINE [2] fileSinkBytes #-}


-- | Write chunks of data to the given file handles.
hSinkBytes 
        :: Handle
        -> IO (Sink IO (Vector F Word8))

hSinkBytes h
 = do   s0      <- G.hSinksBytes [h]
        let Just s1 = wrapI_o s0
        return s1
{-# INLINE [2] hSinkBytes #-}

