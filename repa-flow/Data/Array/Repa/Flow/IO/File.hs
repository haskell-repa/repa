
module Data.Array.Repa.Flow.IO.File
        ( fileSourceBytesUF, hSourceBytesUF
        , fileSinkBytesUF,   hSinkBytesUF)
where
import Data.Array.Repa.Flow.Internals.Base
import Data.Array.Repa.Repr.Unsafe.Foreign
import Data.Array.Repa.Bulk.IO.File
import Data.Array.Repa.Bulk
import System.IO
import Data.Word


-- | Read chunks of data of the given size from a file.
--
--   All chunks have the same size, except possibly the last one.
--
--   TODO: close file when finished.
fileSourceBytesF :: FilePath -> Int -> IO (Source (Vector F Word8))
fileSourceBytesF filePath len
 = do   h       <- openBinaryFile filePath ReadMode
        hSourceBytesUF h len 
{-# INLINE [2] fileSourceBytesUF #-}


-- | Read chunks of data of the given size from the file handle.
--
--   All chunks have the same size, except possibly the last one.
--
hSourceBytesF :: Handle -> Int -> IO (Source (Vector F Word8))
hSourceBytesF h len
 = return $ Source pull_hSource
 where
        pull_hSource eat eject
         = do eof <- hIsEOF h
              if eof 
               then     eject
               else do  !chunk  <- hGetArrayUF h len
                        eat chunk
        {-# INLINE pull_hSource #-}
{-# INLINE [2] hSourceBytesUF #-}


-- | Write chunks of data to the given file.
--
--   TODO: close file when finished.
--
fileSinkBytesF
        :: FilePath -> IO (Sink (Vector UF Word8))

fileSinkBytesF filePath
 = do   h       <- openBinaryFile filePath WriteMode
        hSinkBytesUF h


-- | Write chunks of data to the given file handle.
hSinkBytesF :: Handle -> IO (Sink (Vector F Word8))
hSinkBytesF !h
 = do   let push_hSinkBytesUF !chunk
                = hPutArrayUF h chunk
            {-# NOINLINE push_hSinkBytesUF #-}

        let eject_hSinkBytesUF
                = return ()
            {-# INLINE eject_hSinkBytesUF #-}

        return  $ Sink push_hSinkBytesUF eject_hSinkBytesUF
{-# INLINE [2] hSinkBytesUF #-}
