
-- | Raw Array IO.
module Data.Repa.Array.Auto.IO
        ( -- * Via File Names
          readFile
        , writeFile
        , appendFile

          -- * Via File Handles
        , hGetArray,   hGetArrayPre
        , hPutArray)
where
import Data.Repa.Array.Auto.Base
import Data.Repa.Array.Generic.Convert
import Data.Word
import qualified Data.Repa.Array.Material.Foreign       as A
import qualified Foreign.Ptr                            as F
import qualified Foreign.ForeignPtr                     as F
import qualified Foreign.Marshal.Alloc                  as F
import qualified Foreign.Marshal.Utils                  as F
import qualified System.IO                              as S
import qualified System.FileLock                        as S
import Prelude hiding (readFile, writeFile, appendFile)


---------------------------------------------------------------------------------------------------
-- | Read an entire file as an array of bytes.
readFile :: FilePath -> IO (Array Word8)
readFile path
 = S.withFileLock path S.Shared
 $ \_lock -> do
        h       <- S.openFile path S.ReadMode
        S.hSeek h S.SeekFromEnd  0
        len     <- S.hTell h
        S.hSeek h S.AbsoluteSeek 0
        !arr    <- hGetArray h (fromIntegral len)
        S.hClose h
        return arr
{-# NOINLINE readFile #-}


-- | Write an array of bytes to a file.
writeFile :: FilePath -> Array Word8 -> IO ()
writeFile path arr
 = S.withFileLock path S.Exclusive
 $ \_lock -> do
        h       <- S.openFile path S.WriteMode
        hPutArray h arr
        S.hClose h
{-# NOINLINE writeFile #-}


-- | Append an array of bytes to a file.
appendFile :: FilePath -> Array Word8 -> IO ()
appendFile path arr
 = S.withFileLock path S.Exclusive
 $ \_lock -> do
        h       <- S.openFile path S.AppendMode
        hPutArray h arr
        S.hClose h
{-# NOINLINE appendFile #-}


---------------------------------------------------------------------------------------------------
-- | Get data from a file handle, up to the given number of bytes.
hGetArray :: S.Handle -> Int -> IO (Array Word8)
hGetArray h len
 = do   buf :: F.Ptr Word8 <- F.mallocBytes len
        bytesRead          <- S.hGetBuf h buf len
        fptr               <- F.newForeignPtr F.finalizerFree buf
        return  $! convert $! A.fromForeignPtr bytesRead fptr
{-# NOINLINE hGetArray #-}


-- | Get data from a file handle, up to the given number of bytes, also
--   copying the given data to the front of the new buffer.
hGetArrayPre :: S.Handle -> Int -> Array Word8 -> IO (Array Word8)
hGetArrayPre h len arr
 | (offset, lenPre, fptrPre :: F.ForeignPtr Word8)   
        <- A.toForeignPtr $ convert arr
 = F.withForeignPtr fptrPre
 $ \ptrPre' -> do
        let ptrPre      = F.plusPtr ptrPre' offset
        ptrBuf :: F.Ptr Word8 <- F.mallocBytes (lenPre + len)
        F.copyBytes ptrBuf ptrPre lenPre
        lenRead         <- S.hGetBuf h (F.plusPtr ptrBuf lenPre) len
        let bytesTotal  = lenPre + lenRead
        fptrBuf         <- F.newForeignPtr F.finalizerFree ptrBuf
        return  $ convert $! A.fromForeignPtr bytesTotal fptrBuf
{-# NOINLINE hGetArrayPre #-}


-- | Write data into a file.
hPutArray :: S.Handle -> Array Word8 -> IO ()
hPutArray h arr
 | (offset, lenPre, fptrPre :: F.ForeignPtr Word8)     
        <- A.toForeignPtr $ convert arr
 = F.withForeignPtr fptrPre
 $ \ptr' -> do
        let ptr         = F.plusPtr ptr' offset
        S.hPutBuf h ptr lenPre
{-# NOINLINE hPutArray #-}
