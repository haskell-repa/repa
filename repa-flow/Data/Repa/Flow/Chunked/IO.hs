
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
import Data.Repa.Flow.Chunked.Base
import System.IO
import Data.Word
import Data.Repa.Array.Material.Safe
import Data.Repa.Array.Material.Unsafe.Nested   as A
import Data.Repa.Array                          as A
import qualified Data.Repa.Flow.Generic.IO      as G
#include "repa-stream.h"


-- | Like `fileSourceRecords`, but taking an existing file handle.
sourceRecords 
        :: Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> [Handle]             -- ^ File handles.
        -> IO (Sources Int IO N (Vector F Word8))
sourceRecords = G.sourceRecords
{-# INLINE sourceRecords #-}


-- | Read 8-bit ASCII characters from some files, using the given chunk length.
sourceChars :: Int -> [Handle] -> IO (Sources Int IO F Char)
sourceChars = G.sourceChars
{-# INLINE sourceChars #-}


-- | Read data from some files, using the given chunk length.
sourceBytes :: Int -> [Handle] -> IO (Sources Int IO F Word8)
sourceBytes = G.sourceBytes
{-# INLINE sourceBytes #-}


-- | Write 8-bit ASCII characters to the given file handles.
sinkChars    :: [Handle] -> IO (Sinks Int IO F Char)
sinkChars    =  G.sinkChars
{-# INLINE sinkChars #-}


-- | Write chunks of data to the given file handles.
sinkBytes    :: [Handle] -> IO (Sinks Int IO F Word8)
sinkBytes    =  G.sinkBytes
{-# INLINE sinkBytes #-}

