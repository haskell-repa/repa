
module Data.Array.Repa.Bulk.IO.File
        ( hGetArrayUF
        , hPutArrayUF)
where
import Data.Array.Repa.Repr.Unsafe.Foreign
import Data.Array.Repa.Bulk
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import System.IO
import Data.Word


-- | Get data from a file, up to the given number of bytes.
hGetArrayUF :: Handle -> Int -> IO (Vector UF Word8)
hGetArrayUF h len
 = do
        buf :: Ptr Word8 <- mallocBytes len
        bytesRead        <- hGetBuf h buf len
        fptr             <- newForeignPtr finalizerFree buf
        return  $ fromForeignPtrUF (Z :. bytesRead) fptr
{-# NOINLINE hGetArrayUF #-}


-- | Write data into a file.
hPutArrayUF :: Handle -> Vector UF Word8 -> IO ()
hPutArrayUF h (UFArray _ len fptr)
 = withForeignPtr fptr
 $ \ptr -> do
        hPutBuf h ptr len
{-# NOINLINE hPutArrayUF #-}
