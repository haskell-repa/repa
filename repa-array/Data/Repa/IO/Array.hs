
module Data.Repa.IO.Array
        ( -- * Raw Array IO
          hGetArray,   hGetArrayPre
        , hPutArray

          -- * XSV files
          -- ** Reading
        , getArrayFromXSV,      hGetArrayFromXSV

          -- ** Writing
        , putArrayAsXSV,        hPutArrayAsXSV)
where
import Data.Repa.Fusion.Unpack
import Data.Repa.Array.Material.Boxed
import Data.Repa.Array.Material.Foreign
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


---------------------------------------------------------------------------------------------------
-- | Read a XSV file as a nested array.
--   We get an array of rows:fields:characters.
getArrayFromXSV
        :: Char                 -- ^ Field separator character, eg '|', ',' or '\t'.
        -> FilePath             -- ^ Source file handle.
        -> IO (Array N (Array N (Array F Char)))

getArrayFromXSV !cSep !filePath
 = do   h       <- openFile filePath ReadMode
        arr     <- hGetArrayFromXSV cSep h
        hClose h
        return arr


-- | Read an XSV file as a nested array.
--   We get an array of rows:fields:characters.
hGetArrayFromXSV 
        :: Char                 -- ^ Field separator character, eg '|', ',' or '\t'.
        -> Handle               -- ^ Source file handle.
        -> IO (Array N (Array N (Array F Char)))

hGetArrayFromXSV !cSep !hIn
 = do   
        -- Find out how much data there is remaining in the file.
        start     <- hTell hIn
        hSeek hIn SeekFromEnd 0
        end       <- hTell hIn
        let !len  =  end - start
        hSeek hIn AbsoluteSeek start

        -- Read array as Word8s.
        !arr8   <- hGetArray hIn (fromIntegral len)

        -- Rows are separated by new lines,
        -- fields are separated by the given separator character.
        let !nl = fromIntegral $ ord '\n'
        let !nc = fromIntegral $ ord cSep

        -- Split XSV file into rows and fields.
        -- Convert element data from Word8 to Char.
        -- Chars take 4 bytes each, but are standard Haskell and pretty
        -- print properly. We've done the dicing on the smaller Word8
        -- version, and now map across the elements vector in the array
        -- to do the conversion.
        let !arrChar 
                = A.mapElems 
                        (A.mapElems (A.computeS F . A.map (chr . fromIntegral))) 
                        (A.diceSep nc nl arr8)

        return arrChar


--------------------------------------------------------------------------------------------------
-- | Write a nested array as an XSV file.
--
--   The array contains rows:fields:characters.
putArrayAsXSV
        :: ( BulkI l1 (Array l2 (Array l3 Char))
           , BulkI l2 (Array l3 Char)
           , BulkI l3 Char
           , Unpack (Array l3 Char) t)
        => Char                 -- ^ Separator character, eg '|', ',' or '\t'
        -> FilePath             -- ^ Source file handle.
        -> Array l1 (Array l2 (Array l3 Char))
                                -- ^ Array of row, field, character.
        -> IO ()

putArrayAsXSV !cSep !filePath !arrChar
 = do   h       <- openFile filePath WriteMode
        hPutArrayAsXSV cSep h arrChar
        hClose h


-- | Write a nested array as an XSV file.
--
--   The array contains rows:fields:characters.
hPutArrayAsXSV
        :: ( BulkI l1 (Array l2 (Array l3 Char))
           , BulkI l2 (Array l3 Char)
           , BulkI l3 Char
           , Unpack (Array l3 Char) t)
        => Char                 -- ^ Separator character, eg '|', ',' or '\t'
        -> Handle               -- ^ Source file handle.
        -> Array l1 (Array l2 (Array l3 Char))
                                -- ^ Array of row, field, character.
        -> IO ()

hPutArrayAsXSV !cSep !hOut !arrChar
 = do
        -- Concat result back into Word8s
        let !arrC       = A.fromList U [cSep]
        let !arrNL      = A.fromList U ['\n']

        let !arrOut     
                = A.mapS F (fromIntegral . ord) 
                $ A.concat U 
                $ A.mapS B (\arrFields
                                -> A.concat U $ A.fromList B
                                        [ A.intercalate U arrC arrFields, arrNL])
                $ arrChar

        hPutArray hOut arrOut
{-# INLINE hPutArrayAsXSV #-}

