
module Data.Repa.Flow.Simple.IO
        ( -- * Sourcing Bytes
          fileSourceBytes,     hSourceBytes

          -- * Sourcing Records
        , fileSourceRecords,   hSourceRecords

          -- * Sinking Bytes
        , fileSinkBytes,       hSinkBytes)
where
import Data.Repa.Flow.Simple.Base
import Data.Repa.IO.Array
import Data.Repa.Array.Foreign          as R
import Data.Repa.Array                  as R
import System.IO
import Data.Word


-- Source -----------------------------------------------------------------------------------------
-- | Read chunks of data of the given size from a file.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * All chunks have the same size, except possibly the last one.
--
--   TODO: close file when finished.
fileSourceBytes :: FilePath -> Int -> IO (Source IO (Vector F Word8))
fileSourceBytes filePath len
 = do   h       <- openBinaryFile filePath ReadMode
        hSourceBytes h len 
{-# INLINE [2] fileSourceBytes #-}


-- | Like `fileSourceBytes`, but taking an existing file handle.
hSourceBytes :: Handle -> Int -> IO (Source IO (Vector F Word8))
hSourceBytes h len
 = return $ Source pull_hSource
 where
        pull_hSource eat eject
         = do eof <- hIsEOF h
              if eof 
               then     eject
               else do  !chunk  <- hGetArray h len
                        eat chunk
        {-# INLINE pull_hSource #-}
{-# INLINE [2] hSourceBytes #-}


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
fileSourceRecords 
        :: FilePath 
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> IO (Source IO (Vector F Word8))

fileSourceRecords filePath len pSep aFail
 = do   h       <- openBinaryFile filePath ReadMode
        hSourceRecords h len pSep aFail
{-# INLINE [2] fileSourceRecords #-}


-- | Like `fileSourceRecords`, but taking an existing file handle.
hSourceRecords 
        :: Handle 
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> IO (Source IO (Vector F Word8))

hSourceRecords h len pSep aFail
 = return $ Source pull_hSourceRecordsF
 where  pull_hSourceRecordsF eat eject
         = hIsEOF h >>= \eof
         -> if eof
             -- We're at the end of the file.
             then eject

             -- Read a new chunk from the file.
             else do
                arr      <- hGetArray h len

                -- Find the end of the last record in the file.
                let !mLenSlack  = findIndex pSep (R.reverse arr)

                case mLenSlack of
                 Nothing        -> aFail
                 Just lenSlack
                  -> do let !lenArr     = size (extent arr)
                        let !ixSplit    = lenArr - lenSlack

                        hSeek h RelativeSeek (fromIntegral $ negate lenSlack)
                        let arr'        = window (Z :. 0) (Z :. ixSplit) arr
                        eat arr'
        {-# INLINE pull_hSourceRecordsF #-}
{-# INLINE [2] hSourceRecords #-}


-- Sink -------------------------------------------------------------------------------------------
-- | Write chunks of data to the given file.
--
--   TODO: close file when finished.
--
fileSinkBytes
        :: FilePath -> IO (Sink IO (Vector F Word8))

fileSinkBytes filePath
 = do   h       <- openBinaryFile filePath WriteMode
        hSinkBytes h
{-# NOINLINE fileSinkBytes #-}
--  NOINLINE because the chunks should be big enough to not require fusion,
--           and we don't want to release the code for 'openBinaryFile'.


-- | Write chunks of data to the given file handle.
hSinkBytes :: Handle -> IO (Sink IO (Vector F Word8))
hSinkBytes !h
 = do   let push_hSinkBytesF !chunk
                = hPutArray h chunk
            {-# NOINLINE push_hSinkBytesF #-}

        let eject_hSinkBytesF
                = return ()
            {-# INLINE eject_hSinkBytesF #-}

        return  $ Sink push_hSinkBytesF eject_hSinkBytesF
{-# INLINE [2] hSinkBytes #-}


