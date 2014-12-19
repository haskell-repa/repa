
module Data.Array.Repa.Bulk.IO.File
        ( hGetArrayF
        , hPutArrayF)
where
import Data.Array.Repa.Repr.Foreign
import Data.Array.Repa.Bulk
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import System.IO
import Data.Word


-- | Get data from a file, up to the given number of bytes.
-- 
--   Data read from the file ends up in foreign memory, 
--   which can then be used without copying it into the GHC heap.
hGetArrayF :: Handle -> Int -> IO (Vector F Word8)
hGetArrayF h len
 = do
        buf :: Ptr Word8 <- mallocBytes len
        bytesRead        <- hGetBuf h buf len
        fptr             <- newForeignPtr finalizerFree buf
        return  $ fromForeignPtr (Z :. bytesRead) fptr
{-# NOINLINE hGetArrayF #-}


-- | Write data into a file.
--
--   Data is written to file directly from foreign memory,
--   without copying it through the GHC heap.
hPutArrayF :: Handle -> Vector F Word8 -> IO ()
hPutArrayF h (FArray _ len fptr)
 = withForeignPtr fptr
 $ \ptr -> do
        hPutBuf h ptr len
{-# NOINLINE hPutArrayF #-}
