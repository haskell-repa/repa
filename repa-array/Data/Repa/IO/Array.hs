{-# LANGUAGE ViewPatterns #-}

module Data.Repa.IO.Array
        ( hGetArray,   hGetArrayPre
        , hPutArray
        , hGetArrayFromCSV
        , hPutArrayAsCSV)
where
import Data.Repa.Fusion.Unpack
import Data.Repa.Array.Material.Foreign
import Data.Repa.Array.Material.Boxed           as A
import Data.Repa.Array.Material.Nested          as A
import Data.Repa.Array                          as A
import qualified Foreign.Ptr                    as F
import qualified Foreign.ForeignPtr             as F
import qualified Foreign.Marshal.Alloc          as F
import qualified Foreign.Marshal.Utils          as F
import System.IO
import Data.Word
import Data.Char


-- | Get data from a file, up to the given number of bytes.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--
hGetArray :: Handle -> Int -> IO (Array F Word8)
hGetArray h len
 = do
        buf :: F.Ptr Word8 <- F.mallocBytes len
        bytesRead          <- hGetBuf h buf len
        fptr               <- F.newForeignPtr F.finalizerFree buf
        return  $ fromForeignPtr bytesRead fptr
{-# NOINLINE hGetArray #-}


-- | Get data from a file, up to the given number of bytes, also
--   copying the given data to the front of the new buffer.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--
hGetArrayPre :: Handle -> Int -> Array F Word8 -> IO (Array F Word8)
hGetArrayPre h len (toForeignPtr -> (offset,lenPre,fptrPre))
 = F.withForeignPtr fptrPre
 $ \ptrPre' -> do
        let ptrPre      = F.plusPtr ptrPre' offset
        ptrBuf :: F.Ptr Word8 <- F.mallocBytes (lenPre + len)
        F.copyBytes ptrBuf ptrPre lenPre
        lenRead         <- hGetBuf h (F.plusPtr ptrBuf lenPre) len
        let bytesTotal  = lenPre + lenRead
        fptrBuf         <- F.newForeignPtr F.finalizerFree ptrBuf
        return  $ fromForeignPtr bytesTotal fptrBuf
{-# NOINLINE hGetArrayPre #-}


-- | Write data into a file.
--
--   * Data is written to file directly from foreign memory,
--     without copying it through the GHC heap.
--
hPutArray :: Handle -> Array F Word8 -> IO ()
hPutArray h (toForeignPtr -> (offset,lenPre,fptr))
 = F.withForeignPtr fptr
 $ \ptr' -> do
        let ptr         = F.plusPtr ptr' offset
        hPutBuf h ptr lenPre
{-# NOINLINE hPutArray #-}


-- | Read a CSV file as a nested array.
--   We get an array of rows:fields:characters.
--
hGetArrayFromCSV 
        :: Handle 
        -> IO (Array N (Array N (Array F Char)))

hGetArrayFromCSV hIn
 = do   
        -- Find out how much data there is remaining in the file.
        start   <- hTell hIn
        hSeek hIn SeekFromEnd 0
        end     <- hTell hIn
        let !len        = end - start
        hSeek hIn AbsoluteSeek start

        -- Read array as Word8s.
        arr8    <- hGetArray hIn (fromIntegral len)

        -- Rows are separated by new lines, fields are separated by commas.
        let !nc = fromIntegral $ ord ','
        let !nl = fromIntegral $ ord '\n'
        let !nr = fromIntegral $ ord '\r'

        let arrSep :: Array N (Array N (Array F Word8)) 
                = A.diceSep nc nl arr8

        -- Split TSV file into rows and fields.
        -- Convert element data from Word8 to Char.
        -- Chars take 4 bytes each, but are standard Haskell and pretty
        -- print properly. We've done the dicing on the smaller Word8
        -- version, and now map across the elements vector in the array
        -- to do the conversion.
        let arrChar :: Array N (Array N (Array F Char))
                = A.mapElems 
                        (A.mapElems (A.computeS F . A.map (chr . fromIntegral))) 
                        arrSep

        return arrChar


-- | Write a nested array as a CSV file.
--   The array contains rows:fields:characters.
--
hPutArrayAsCSV 
        :: ( BulkI l1 (Array l2 (Array l3 Char))
           , BulkI l2 (Array l3 Char)
           , BulkI l3 Char
           , Unpack (Array l3 Char) t)
        => Handle
        -> Array l1 (Array l2 (Array l3 Char))
        -> IO ()

hPutArrayAsCSV hOut arrChar
 = do
        -- Concat result back into Word8s
        let !arrC       = A.fromList U [',']
        let !arrNL      = A.fromList U ['\n']

        let !arrOut     
                = A.mapS F (fromIntegral . ord) 
                $ A.concat U 
                $ A.mapS B (\arrFields
                                -> A.concat U $ A.fromList B
                                        [ A.intercalate U arrC arrFields, arrNL])
                $ arrChar

        hPutArray hOut arrOut
{-# INLINE hPutArrayAsCSV #-}

