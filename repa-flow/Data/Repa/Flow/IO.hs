
-- | Flow file IO.
module Data.Repa.Flow.IO
        ( -- * Sourcing records
          fileSourcesRecords
        , hSourcesRecords

          -- * Sourcing lines
        , fileSourcesLines
        , hSourcesLines

          -- * Sourcing bytes
        , fileSourcesBytes
        , hSourcesBytes

          -- * Sinking bytes
        , hSinksBytes
        , fileSinksBytes)
where
import Data.Repa.Flow
import Data.Repa.Eval.Array                     as A
import Data.Repa.Array                          as A hiding (fromList, fromLists)
import qualified Data.Repa.Flow.Generic         as G hiding (next)
import System.IO
import Data.Word
import Data.Char


-- Source Records -------------------------------------------------------------
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
--   Each file will be closed the first time the consumer tries to pull an element
--   from the associated stream when no more are available.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * The provided file handle must support seeking, else you'll get an exception.
--
fileSourcesRecords 
        :: [FilePath]           -- ^ File paths.
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a
                                --   whole record.
        -> IO (Sources UN (Vector F Word8))
fileSourcesRecords = G.fileSourcesRecords
{-# INLINE fileSourcesRecords #-}


-- | Like `fileSourcesRecords`, but taking existing file handles.
hSourcesRecords 
        :: [Handle]             --  File handles.
        -> Int                  --  Size of chunk to read in bytes.
        -> (Word8 -> Bool)      --  Detect the end of a record.        
        -> IO ()                --  Action to perform if we can't get a
                                --   whole record.
        -> IO (Sources UN (Vector F Word8))
hSourcesRecords = G.hSourcesRecords
{-# INLINE hSourcesRecords #-}


-- Source Lines ---------------------------------------------------------------
-- | Read complete lines of data from a text file, using the given chunk length.
--
--   * The trailing new-line characters are discarded.
--   * Chunk data appears in foreign memory, without going via the GHC heap.
--
fileSourcesLines 
        :: [FilePath]           -- ^ File paths.
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> IO ()                -- ^ Action to perform if we can't get a
                                --   whole record.
        -> IO (Sources UN (Vector F Char))
fileSourcesLines files nChunk fails
 =   mapChunks_i chopChunk
 =<< G.fileSourcesRecords files nChunk isNewLine fails
 where  
        isNewLine   :: Word8 -> Bool
        isNewLine x =  x == nl
        {-# INLINE isNewLine #-}
  
        chopChunk chunk
         = A.mapElems (A.computeS_ . A.map (chr . fromIntegral)) 
         $ A.trimEnds (== nl) chunk
        {-# INLINE chopChunk #-}

        nl :: Word8
        !nl = fromIntegral $ ord '\n'
{-# INLINE fileSourcesLines #-}


-- | Like `fileSourcesLines`, but taking existing file handles.
hSourcesLines
        :: [Handle]             --  File handles.
        -> Int                  --  Size of chunk to read in bytes.
        -> IO ()                --  Action to perform if we can't get a
                                --   whole record.
        -> IO (Sources UN (Vector F Char))
hSourcesLines hs nChunk fails
 =   mapChunks_i chopChunk
 =<< G.hSourcesRecords hs nChunk isNewLine fails
 where
        isNewLine   :: Word8 -> Bool
        isNewLine x =  x == nl
        {-# INLINE isNewLine #-}
  
        chopChunk chunk
         = A.mapElems (A.computeS_ . A.map (chr . fromIntegral)) 
         $ A.trimEnds (== nl) chunk
        {-# INLINE chopChunk #-}

        nl :: Word8
        !nl = fromIntegral $ ord '\n'
{-# INLINE hSourcesLines #-}


-- Source Bytes ---------------------------------------------------------------
-- | Read data from some files, using the given chunk length.
fileSourcesBytes 
        :: [FilePath]  -> Int 
        -> IO (Sources F Word8)
fileSourcesBytes = G.fileSourcesBytes
{-# INLINE fileSourcesBytes #-}


-- | Like `fileSourcesBytes`, but taking existing file handles.
hSourcesBytes 
        :: [Handle]   -> Int 
        -> IO (Sources F Word8)
hSourcesBytes = G.hSourcesBytes
{-# INLINE hSourcesBytes #-}


-- Sink Bytes -----------------------------------------------------------------
-- | Write data to the given files.
fileSinksBytes :: [FilePath] -> IO (Sinks F Word8)
fileSinksBytes = G.fileSinksBytes
{-# INLINE fileSinksBytes #-}


-- | Write chunks of data to the given file handles.
hSinksBytes    :: [Handle]   -> IO (Sinks F Word8)
hSinksBytes    = G.hSinksBytes
{-# INLINE hSinksBytes #-}

