
-- | Input and Output for Chunked Flows.
--
--   Most functions in this module are re-exports of the ones from
--   "Data.Repa.Flow.Generic.IO", but using the `Sources` and `Sinks`
--   type synonyms for chunked flows.
--
module Data.Repa.Flow.Chunked.IO
        ( -- * Sourcing Bytes
          sourceBytes,          hSourceBytes

          -- * Sourcing Records
        , sourceRecords,        hSourceRecords

          -- * Sinking Bytes
        , sinkBytes,            hSinkBytes)
where
import Data.Repa.Flow.Chunked.Base
import System.IO
import Data.Word
import Data.Repa.Array.Foreign                  as A
import Data.Repa.Array                          as A
import qualified Data.Repa.Flow.Generic.IO      as G


-- | Read data from some files, using the given chunk length.
--
--   * Chunk data appears in foreign memory, without copying it into the
--     GHC heap.
-- 
sourceBytes 
        :: [FilePath]  -> Int 
        -> IO (Sources Int IO F Word8)
sourceBytes = G.sourceBytes
{-# INLINE [2] sourceBytes #-}


-- | Like `sourceBytes`, but taking existing file handles.
hSourceBytes 
        :: [Handle]   -> Int 
        -> IO (Sources Int IO F Word8)
hSourceBytes = G.hSourceBytes
{-# INLINE [2] hSourceBytes #-}


-- | Read complete records of data from a file, using the given chunk length.
sourceRecords 
        :: [FilePath]           -- ^ File paths.
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> IO (Sources Int IO UN (Vector F Word8))
sourceRecords = G.sourceRecords
{-# INLINE [2] sourceRecords #-}


-- | Like `fileSourceRecords`, but taking an existing file handle.
hSourceRecords 
        :: [Handle]             -- ^ File handles.
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> IO (Sources Int IO UN (Vector F Word8))
hSourceRecords = G.hSourceRecords
{-# INLINE [2] hSourceRecords #-}


-- | Write data to the given files.
sinkBytes :: [FilePath] -> IO (Sinks Int IO F Word8)
sinkBytes =  G.sinkBytes
{-# INLINE [2] sinkBytes #-}


-- | Write chunks of data to the given file handles.
hSinkBytes    :: [Handle] -> IO (Sinks Int IO F Word8)
hSinkBytes    =  G.hSinkBytes
{-# INLINE [2] hSinkBytes #-}
