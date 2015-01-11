
-- | Input and Output for Chunked Flows.
--
--   Most functions in this module are re-exports of the ones from
--   "Data.Repa.Flow.Generic.IO", but using the `Sources` and `Sinks`
--   type synonyms for chunked flows.
--
module Data.Repa.Flow.Chunked.IO
        ( -- * Sourcing Bytes
          fileSourcesBytes,     hSourcesBytes

          -- * Sourcing Records
        , fileSourcesRecords,   hSourcesRecords

          -- * Sinking Bytes
        , fileSinksBytes,       hSinksBytes)
where
import Data.Repa.Flow.Chunked.Base
import System.IO
import Data.Word
import Data.Repa.Array.Foreign                  as R
import qualified Data.Repa.Flow.Generic.IO      as G


-- | Read data from some files, using the given chunk length.
--
--   * Chunk data appears in foreign memory, without copying it into the
--     GHC heap.
-- 
fileSourcesBytes 
        :: [FilePath]  -> Int 
        -> IO (Sources Int IO F Word8)
fileSourcesBytes = G.fileSourcesBytes
{-# INLINE [2] fileSourcesBytes #-}


-- | Like `fileSourceBytes`, but taking existing file handles.
hSourcesBytes 
        :: [Handle]   -> Int 
        -> IO (Sources Int IO F Word8)
hSourcesBytes = G.hSourcesBytes
{-# INLINE [2] hSourcesBytes #-}


-- | Read complete records of data from a file, using the given chunk length.
fileSourcesRecords 
        :: [FilePath]           -- ^ File paths.
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> IO (Sources Int IO F Word8)
fileSourcesRecords = G.fileSourcesRecords
{-# INLINE [2] fileSourcesRecords #-}


-- | Like `fileSourceRecords`, but taking an existing file handle.
hSourcesRecords 
        :: [Handle]             -- ^ File handles.
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> IO (Sources Int IO F Word8)
hSourcesRecords = G.hSourcesRecords
{-# INLINE [2] hSourcesRecords #-}


-- | Write data to the given files.
fileSinksBytes :: [FilePath] -> IO (Sinks Int IO F Word8)
fileSinksBytes =  G.fileSinksBytes
{-# INLINE [2] fileSinksBytes #-}


-- | Write chunks of data to the given file handles.
hSinksBytes    :: [Handle] -> IO (Sinks Int IO F Word8)
hSinksBytes    =  G.hSinksBytes
{-# INLINE [2] hSinksBytes #-}
