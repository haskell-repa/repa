
-- | Read and write files.
--
--   The functions in this module are wrappers for the ones in 
--   "Data.Repa.Flow.Default.SizedIO" that use a default chunk size of
--   64kBytes and just call `error` if the source file appears corruped. 
--
module Data.Repa.Flow.Auto.IO
        ( defaultChunkSize

          -- * Buckets
        , module Data.Repa.Flow.IO.Bucket

          -- * Sourcing
        , sourceBytes
        , sourceChars
        , sourceLines
        , sourceRecords
        , sourceTSV
        , sourceCSV

          -- * Sinking
        , sinkBytes
        , sinkLines
        , sinkChars
        )
where
import Data.Repa.Flow.Auto
import Data.Repa.Flow.IO.Bucket
import Data.Repa.Array.Material                 as A
import Data.Word
import qualified Data.Repa.Flow.Auto.SizedIO    as F
#include "repa-flow.h"


-- | The default chunk size of 64kBytes.
defaultChunkSize :: Integer
defaultChunkSize = 64 * 1024


-- | Read a file containing Comma-Separated-Values.
sourceCSV :: Array B Bucket 
          -> IO (Sources (Array A (Array A Char)))
sourceCSV
        = F.sourceCSV defaultChunkSize
        $ error $  "Line exceeds chunk size of "
                ++ show defaultChunkSize ++ "bytes."
{-# INLINE sourceCSV #-}


-- | Read a file containing Tab-Separated-Values.
sourceTSV :: Array B Bucket 
          -> IO (Sources (Array A (Array A Char)))
sourceTSV
        = F.sourceTSV defaultChunkSize
        $ error $  "Line exceeds chunk size of "
                ++ show defaultChunkSize ++ "bytes."
{-# INLINE sourceTSV #-}


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
        :: (Word8 -> Bool)      -- ^ Detect the end of a record.
        -> Array B Bucket       -- ^ Buckets.
        -> IO (Sources (Array A Word8))
sourceRecords pSep 
        = F.sourceRecords defaultChunkSize pSep
        $ error $  "Record exceeds chunk size of " 
                ++ show defaultChunkSize ++ "bytes."
{-# INLINE sourceRecords #-}


-- | Read complete lines of data from a text file, using the given chunk length.
--   We read as many complete lines as will fit into each chunk.
--
--   * The trailing new-line characters are discarded.
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * The provided file handle must support seeking, else you'll get an
--     exception.
--   * Each file is closed the first time the consumer tries to pull a line
--     from the associated stream when no more are available.
--
sourceLines 
        :: Array B Bucket -> IO (Sources (Array A Char))
sourceLines     
        = F.sourceLines   defaultChunkSize
        $ error $  "Line exceeds chunk size of "
                ++ show defaultChunkSize ++ "bytes."
{-# INLINE sourceLines #-}


-- | Read 8-bit ASCII characters from some files, using the given chunk length.
sourceChars :: Array B Bucket -> IO (Sources Char)
sourceChars = F.sourceChars defaultChunkSize
{-# INLINE sourceChars #-}


-- | Read data from some files, using the given chunk length.
sourceBytes :: Array B Bucket -> IO (Sources Word8)
sourceBytes = F.sourceBytes defaultChunkSize
{-# INLINE sourceBytes #-}


-- | Write vectors of text lines to the given files handles.
-- 
--   * Data is copied into a new buffer to insert newlines before being
--     written out.
--
sinkLines :: Array B Bucket -> IO (Sinks (Array A Char))
sinkLines = F.sinkLines
{-# INLINE sinkLines #-}


-- | Write 8-bit ASCII characters to some files.
sinkChars :: Array B Bucket -> IO (Sinks Char)
sinkChars =  F.sinkChars
{-# INLINE sinkChars #-}


-- | Write bytes to some file.
sinkBytes :: Array B Bucket -> IO (Sinks Word8)
sinkBytes =  F.sinkBytes
{-# INLINE sinkBytes #-}
