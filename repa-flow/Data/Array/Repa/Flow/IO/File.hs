
module Data.Array.Repa.Flow.IO.File
        ( fileSourceBytesF, hSourceBytesF
                          , hSourceRecordsF
        , fileSinkBytesF,   hSinkBytesF)
where
import Data.Array.Repa.Flow.Internals.Base
import Data.Array.Repa.Repr.Foreign
import Data.Array.Repa.Bulk.IO.File
import Data.Array.Repa.Bulk
import System.IO
import Data.Word
import Data.IORef


-- Source -----------------------------------------------------------------------------------------
-- | Read chunks of data of the given size from a file.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * All chunks have the same size, except possibly the last one.
--
--   TODO: close file when finished.
fileSourceBytesF :: FilePath -> Int -> IO (Source (Vector F Word8))
fileSourceBytesF filePath len
 = do   h       <- openBinaryFile filePath ReadMode
        hSourceBytesF h len 
{-# INLINE [2] fileSourceBytesF #-}


-- | Read chunks of data of the given size from the file handle.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * All chunks have the same size, except possibly the last one.
--
hSourceBytesF :: Handle -> Int -> IO (Source (Vector F Word8))
hSourceBytesF h len
 = return $ Source pull_hSource
 where
        pull_hSource eat eject
         = do eof <- hIsEOF h
              if eof 
               then     eject
               else do  !chunk  <- hGetArrayF h len
                        eat chunk
        {-# INLINE pull_hSource #-}
{-# INLINE [2] hSourceBytesF #-}


-- | Read complete records of data from a file handle, 
--   up to the given size in bytes.
--
--   The records are separated with the given terminating character.
--   After reading some records from the file we seek to just after
--   the last record which was read, so we can continue to read
--   more complete records next time.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * All chunks have the same size, except possibly the last one.
--   * The provided file handle must support seeking, else you'll get an exception.
-- 
hSourceRecordsF :: Handle -> Int -> Word8 -> IO (Source (Vector F Word8))
hSourceRecordsF h len _sep 
 = do
        rSpill  <- newIORef Nothing

        let pull_hSourceRecordsF eat eject
             = do eof <- hIsEOF h
                  if eof
                   then do
                        mspill   <- readIORef rSpill
                        case mspill of
                         Nothing    -> eject
                         Just chunk -> do _ <- eat chunk
                                          eject

                   else do
                        mspill   <- readIORef rSpill
                        case mspill of
                         Nothing    
                          -> do arr <- hGetArrayF h len
                                eat arr

                         Just arrSpill
                          -> do arr <- hGetArrayPreF h len arrSpill
                                eat arr
            {-# INLINE pull_hSourceRecordsF #-}

        return $ Source pull_hSourceRecordsF
{-# INLINE [2] hSourceRecordsF #-}


-- Sink -------------------------------------------------------------------------------------------
-- | Write chunks of data to the given file.
--
--   TODO: close file when finished.
--
fileSinkBytesF
        :: FilePath -> IO (Sink (Vector F Word8))

fileSinkBytesF filePath
 = do   h       <- openBinaryFile filePath WriteMode
        hSinkBytesF h
{-# INLINE [2] fileSinkBytesF #-}


-- | Write chunks of data to the given file handle.
hSinkBytesF :: Handle -> IO (Sink (Vector F Word8))
hSinkBytesF !h
 = do   let push_hSinkBytesF !chunk
                = hPutArrayF h chunk
            {-# NOINLINE push_hSinkBytesF #-}

        let eject_hSinkBytesF
                = return ()
            {-# INLINE eject_hSinkBytesF #-}

        return  $ Sink push_hSinkBytesF eject_hSinkBytesF
{-# INLINE [2] hSinkBytesF #-}


