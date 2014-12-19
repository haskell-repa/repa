
module Data.Array.Repa.Bulk.IO.File
        ( hGetArrayF,   hGetArrayPreF
        , hPutArrayF)
where
import Data.Array.Repa.Repr.Foreign
import Data.Array.Repa.Bulk
import qualified Foreign.Ptr            as F
import qualified Foreign.ForeignPtr     as F
import qualified Foreign.Marshal.Alloc  as F
import qualified Foreign.Marshal.Utils  as F
import System.IO
import Data.Word


-- | Get data from a file, up to the given number of bytes.
-- 
--   * Data is read into foreign memory without copying it through the GHC heap.
--
hGetArrayF :: Handle -> Int -> IO (Vector F Word8)
hGetArrayF h len
 = do
        buf :: F.Ptr Word8 <- F.mallocBytes len
        bytesRead          <- hGetBuf h buf len
        fptr               <- F.newForeignPtr F.finalizerFree buf
        return  $ fromForeignPtr (Z :. bytesRead) fptr
{-# NOINLINE hGetArrayF #-}


-- | Get data from a file, up to the given number of bytes, also 
--   copying the given data to the front of the new buffer.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--
hGetArrayPreF :: Handle -> Int -> Vector F Word8 -> IO (Vector F Word8)
hGetArrayPreF h len (FArray shPre offset fptrPre)
 = F.withForeignPtr fptrPre
 $ \ptrPre' -> do   
        let ptrPre      = F.plusPtr ptrPre' offset
        let lenPre      = size shPre
        ptrBuf :: F.Ptr Word8 <- F.mallocBytes (lenPre + len)
        F.copyBytes ptrBuf ptrPre lenPre
        lenRead         <- hGetBuf h (F.plusPtr ptrBuf lenPre) len
        let bytesTotal  = lenPre + lenRead
        fptrBuf         <- F.newForeignPtr F.finalizerFree ptrBuf
        return  $ fromForeignPtr (Z :. bytesTotal) fptrBuf
{-# NOINLINE hGetArrayPreF #-}


-- | Write data into a file.
--
--   * Data is written to file directly from foreign memory,
--     without copying it through the GHC heap.
--
hPutArrayF :: Handle -> Vector F Word8 -> IO ()
hPutArrayF h (FArray shPre offset fptr)
 = F.withForeignPtr fptr
 $ \ptr' -> do
        let ptr         = F.plusPtr ptr' offset
        hPutBuf h ptr (size shPre)
{-# NOINLINE hPutArrayF #-}
