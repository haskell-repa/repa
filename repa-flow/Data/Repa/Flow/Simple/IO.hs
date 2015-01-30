
module Data.Repa.Flow.Simple.IO
        ( G.fromFiles
        , sourceBytes
        , sourceRecords
        , G.toFiles
        , sinkBytes)
where
import Data.Repa.Flow.Simple.Base
import System.IO
import Data.Word
import Data.Repa.Array.Material                 as A
import qualified Data.Repa.Flow.Generic.IO      as G
#include "repa-stream.h"


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
        :: Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> Handle               -- ^ File handle.
        -> IO (Source IO (Array N (Array F Word8)))

sourceRecords len pSep aFail h
 = do   s0      <- G.sourceRecords len pSep aFail [h]
        let Just s1 = wrapI_i s0
        return s1
{-# INLINE sourceRecords #-}


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
        :: Int -> Handle 
        -> IO (Source IO (Array F Word8))

sourceBytes len h
 = do   s0      <- G.sourceBytes len [h]
        let Just s1 = wrapI_i s0
        return s1
{-# INLINE sourceBytes #-}


-- Sinking Bytes ----------------------------------------------------------------------------------
-- | Write chunks of data to the given files.
--
--   The file will be closed when the associated stream is ejected.
--
sinkBytes :: Handle  -> IO (Sink IO (Array F Word8))
sinkBytes h
 = do   s0      <- G.sinkBytes [h]
        let Just s1 = wrapI_o s0
        return s1
{-# INLINE sinkBytes #-}

