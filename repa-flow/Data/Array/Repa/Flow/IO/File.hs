
module Data.Array.Repa.Flow.IO.File
        ( -- * Sourcing Bytes
          fileSourceBytesF,     hSourceBytesF

          -- * Sourcing Records
        , fileSourceRecordsF,   hSourceRecordsF

          -- * Sinking Bytes
        , fileSinkBytesF,       hSinkBytesF)
where
import Data.Array.Repa.Flow.Internals.Base
import Data.Array.Repa.Repr.Foreign
import Data.Array.Repa.Bulk.IO.File
import Data.Array.Repa.Bulk                     as R
import System.IO
import Data.Word


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


-- | Like `fileSourceBytesF`, but taking an existing file handle.
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


-- | Read complete records of data from a file, up to the given size in bytes.
--
--   The records are separated by a special terminating character, which the 
--   given predicate detects. After reading a chunk of data we seek to just after the
--   last complete record that was read, so we can continue to read more complete
--   records next time.
--
--   If we cannot find an end-of-record terminator then apply the given failure 
--   action.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * All chunks have the same size, except possibly the last one.
--   * The provided file handle must support seeking, else you'll get an exception.
-- 
--   TODO: close file when done.
--
fileSourceRecordsF 
        :: FilePath 
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> IO (Source (Vector F Word8))

fileSourceRecordsF filePath len pSep aFail
 = do   h       <- openBinaryFile filePath ReadMode
        hSourceRecordsF h len pSep aFail
{-# INLINE [2] fileSourceRecordsF #-}


-- | Like `fileSourceRecordsF`, but taking an existing file handle.
hSourceRecordsF 
        :: Handle 
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> IO (Source (Vector F Word8))

hSourceRecordsF h len pSep aFail
 = return $ Source pull_hSourceRecordsF
 where  pull_hSourceRecordsF eat eject
         = hIsEOF h >>= \eof
         -> if eof
             -- We're at the end of the file.
             then eject

             -- Read a new chunk from the file.
             else do
                arr      <- hGetArrayF h len

                -- Find the end of the last record in the file.
                let !mIxSplit = findIndex pSep (R.reverse arr)

                case mIxSplit of
                 Nothing        -> aFail
                 Just ixSplit
                  -> do let lenSplit    = size (extent arr) - ixSplit
                        hSeek h RelativeSeek (fromIntegral $ negate lenSplit)
                        let arr'        = slice (Z :. 0) (Z :. lenSplit) arr
                        eat arr'
        {-# INLINE pull_hSourceRecordsF #-}

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


