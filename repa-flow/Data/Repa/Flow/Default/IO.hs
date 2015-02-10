
-- | Read and write files.
--
--   The functions in this module are wrappers for the ones in 
--   "Data.Repa.Flow.Default.SizedIO" that use a default chunk size of
--   64kBytes and just call `error` if the source file appears corruped. 
module Data.Repa.Flow.Default.IO
        ( defaultChunkSize

          -- * Buckets
        , module Data.Repa.Flow.IO.Bucket

          -- * Sourcing
        , F.fromFiles
        , sourceTSV
        , sourceRecords
        , sourceLines
        , sourceChars
        , sourceBytes

          -- * Sinking
        , F.toFiles
        , sinkChars
        , sinkLines
        , sinkBytes)
where
import Data.Repa.Flow.Default
import Data.Repa.Flow.IO.Bucket
import Data.Repa.Fusion.Unpack
import Data.Word
import qualified Data.Repa.Flow.Default.SizedIO  as F
#include "repa-stream.h"


-- | The default chunk size of 64kBytes.
defaultChunkSize :: Integer
defaultChunkSize = 64 * 1024


-- | Read a file containing Tab-Separated-Values.
sourceTSV
        :: [Bucket] -> IO (Sources N (Array N (Array F Char)))
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
        -> [Bucket]             -- ^ Buckets.
        -> IO (Sources N (Array F Word8))
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
        :: [Bucket] -> IO (Sources N (Array F Char))
sourceLines     
        = F.sourceLines   defaultChunkSize
        $ error $  "Line exceeds chunk size of "
                ++ show defaultChunkSize ++ "bytes."
{-# INLINE sourceLines #-}


-- | Read 8-bit ASCII characters from some files, using the given chunk length.
sourceChars 
        :: [Bucket] -> IO (Sources F Char)
sourceChars     = F.sourceChars defaultChunkSize
{-# INLINE sourceChars #-}


-- | Read data from some files, using the given chunk length.
sourceBytes 
        :: [Bucket] -> IO (Sources F Word8)
sourceBytes     = F.sourceBytes defaultChunkSize
{-# INLINE sourceBytes #-}


-- | Write vectors of text lines to the given files handles.
-- 
--   * Data is copied into a new buffer to insert newlines before being
--     written out.
--
sinkLines 
        :: ( BulkI l1 (Array l2 Char)
           , BulkI l2 Char, Unpack (Array l2 Char) t2)
        => Name l1              -- ^ Layout of chunks.
        -> Name l2              -- ^ Layout of lines in chunks.
        -> [Bucket]             -- ^ Buckets
        -> IO (Sinks l1 (Array l2 Char))
sinkLines       = F.sinkLines
{-# INLINE sinkLines #-}


-- | Write 8-bit ASCII characters to some files.
sinkChars 
        :: [Bucket] -> IO (Sinks F Char)
sinkChars =  F.sinkChars
{-# INLINE sinkChars #-}


-- | Write bytes to some file.
sinkBytes 
        :: [Bucket] -> IO (Sinks F Word8)
sinkBytes =  F.sinkBytes
{-# INLINE sinkBytes #-}
