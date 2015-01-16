
-- | Input and Output for Chunked Flows.
--
--   Most functions in this module are re-exports of the ones from
--   "Data.Repa.Flow.Generic.IO", but using the `Sources` and `Sinks`
--   type synonyms for chunked flows.
--
module Data.Repa.Flow.Chunked.IO
        ( sourceBytes
        , sourceRecords
        , sinkBytes)
where
import Data.Repa.Flow.Chunked.Base
import System.IO
import Data.Word
import Data.Repa.Array.Foreign                  as A
import Data.Repa.Array                          as A
import qualified Data.Repa.Flow.Generic.IO      as G


-- | Read data from some files, using the given chunk length.
--
-- | Like `sourceBytes`, but taking existing file handles.
sourceBytes 
        :: Int -> [Handle]
        -> IO (Sources Int IO F Word8)
sourceBytes = G.sourceBytes
{-# INLINE [2] sourceBytes #-}


-- | Like `fileSourceRecords`, but taking an existing file handle.
sourceRecords 
        :: Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> [Handle]             -- ^ File handles.
        -> IO (Sources Int IO UN (Vector F Word8))
sourceRecords = G.sourceRecords
{-# INLINE [2] sourceRecords #-}


-- | Write chunks of data to the given file handles.
sinkBytes    :: [Handle] -> IO (Sinks Int IO F Word8)
sinkBytes    =  G.sinkBytes
{-# INLINE [2] sinkBytes #-}

