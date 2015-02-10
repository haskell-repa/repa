
-- | Input and Output for Chunked Flows.
--
--   Most functions in this module are re-exports of the ones from
--   "Data.Repa.Flow.Generic.IO", but using the `Sources` and `Sinks`
--   type synonyms for chunked flows.
--
module Data.Repa.Flow.Chunked.IO
        ( -- * Sourcing
          sourceRecords
        , sourceChars
        , sourceBytes

          -- * Sinking
        , sinkChars
        , sinkBytes)
where
import Data.Repa.Flow.IO.Bucket
import Data.Repa.Flow.Chunked.Base
import Data.Repa.Array                          as A
import Data.Repa.Array.Material                 as A
import qualified Data.Repa.Flow.Generic.IO      as G
import Data.Word
#include "repa-stream.h"


-- | Like `fileSourceRecords`, but taking an existing file handle.
sourceRecords 
        :: BulkI l Bucket
        => Integer              -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> Array l Bucket       -- ^ File handles.
        -> IO (Sources Int IO N (Array F Word8))
sourceRecords = G.sourceRecords
{-# INLINE sourceRecords #-}


-- | Read 8-bit ASCII characters from some files, using the given chunk length.
sourceChars 
        :: BulkI l Bucket
        => Integer -> Array l Bucket -> IO (Sources Int IO F Char)
sourceChars = G.sourceChars
{-# INLINE sourceChars #-}


-- | Read data from some files, using the given chunk length.
sourceBytes 
        :: BulkI l Bucket
        => Integer -> Array l Bucket -> IO (Sources Int IO F Word8)
sourceBytes = G.sourceBytes
{-# INLINE sourceBytes #-}


-- | Write 8-bit ASCII characters to the given file handles.
sinkChars :: BulkI l Bucket
          => Array l Bucket -> IO (Sinks Int IO F Char)
sinkChars =  G.sinkChars
{-# INLINE sinkChars #-}


-- | Write chunks of data to the given file handles.
sinkBytes :: BulkI l Bucket
          => Array l Bucket -> IO (Sinks Int IO F Word8)
sinkBytes =  G.sinkBytes
{-# INLINE sinkBytes #-}

