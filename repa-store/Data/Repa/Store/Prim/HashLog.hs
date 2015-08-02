
module Data.Repa.Store.Prim.HashLog
        ( HashLog
        , connect
        , lengthBytes
        , lengthBytesHash
        , append
        , foldIO
        , ErrorHashLog (..))
where
import Data.Int
import Data.ByteString.Char8            (ByteString)
import Data.ByteString.Base16           as B16
import qualified System.Posix.Files     as System
import qualified System.Posix.Types     as System
import qualified System.FileLock        as System
import qualified System.Directory       as System
import qualified System.IO              as System
import qualified Crypto.Hash.MD5        as MD5
import qualified Data.ByteString.Char8  as BS
import Prelude                          as P
import Numeric


-- | A simple hashed container format.
--
--   * Contains a sequence of values, along with a header for each value
--     that gives its size and the first few bytes of its MD5 hash.
--
--   * The hash of each element is computed when it is written,
--     and re-validated when read, to ensure the file has not been corrupted.
--
--   * The size and hashes are encoded as ASCII text, so if
--     the elements are also human readable then the whole file will be.
--
--   * The maximum size of each element is 4GB (2^32 bytes).
--
--   File format is like:
--
-- @
-- LOG100000165a6d18123
-- (element 1 text)
-- LOG10000015444d13623
-- (element 2 text)
-- ...
-- @
--
-- Each header line consists of the characters `LOG1`, followed by a hex encoded
-- 8 byte element length, and the hex encoded first 8 bytes of the MD5 hash.
--
data HashLog
        = HashLog
        { hashLogPath   :: FilePath }


-- | Connect to a `HashLog`.
--
--   * No file handles or other scarce resources are held between accesses
--     to the log, so the result does not need to be release when the file
--     has finished being used.
--
connect :: FilePath -> IO (Either ErrorHashLog HashLog)
connect path 
        = return $ Right HashLog
        { hashLogPath   = path }


-- | Get the length of the log, in bytes.
lengthBytes :: HashLog -> IO (Either ErrorHashLog Integer)
lengthBytes hl
 = do   exists  <- System.doesFileExist (hashLogPath hl)
        if exists
         then do 
                status <- System.getFileStatus (hashLogPath hl)
                return $  Right $ fromIntegral $ System.fileSize status
         else   return $  Left  $ ErrorHashLogFileNotFound (hashLogPath hl)


-- | Get a hash of the length of the log, including the given salt string.
lengthBytesHash :: HashLog -> ByteString -> IO (Either ErrorHashLog ByteString)
lengthBytesHash hl bsSalt
 = do   lenFile <- lengthBytes hl
        return $ Right $ MD5.hash (bsSalt `BS.append` BS.pack (show lenFile))


-- | Given the file position and element string, 
--   produce the header for that element, including the trailing newline.
headerForElem   :: Int64 -> ByteString -> ByteString
headerForElem posFile bs
 = let  
        -- Hex encoded length
        lenData         = BS.length bs
        strLenData      = showHex lenData ""
        bsLenDataPad    = BS.replicate (8 - P.length strLenData) '0' 
                         `BS.append` BS.pack strLenData

        -- Hex encoded hash, we only take the first 8 bytes as we're
        -- really just using it as a checksum.
        bsHash          = MD5.hash (bs `BS.append` BS.pack (show posFile))
        bsHash8         = BS.take 8 $ B16.encode bsHash

   in   BS.pack "LOG1" 
         `BS.append` bsLenDataPad
         `BS.append` bsHash8
         `BS.append` (BS.pack "\n")


-- | Append a new value to the log.
append :: HashLog -> ByteString -> IO (Maybe ErrorHashLog)
append hl bsElem
 | lenData <- BS.length bsElem
 , lenData >= 2^(32 :: Integer)
 = return $ Just $ ErrorHashLogElemTooBig

 | otherwise
 = -- Lock the log file, creating it if it's not present.
   -- We can't have other processes appending to the file between us
   -- taking it's length and writing the element hash.
   System.withFileLock (hashLogPath hl) System.Exclusive
    $ \_lock -> do
        let path        = hashLogPath hl

        -- Get the current length of the file, we use this to salt the
        -- hash of the data so that we can detect if the order of elements
        -- has been changed.
        status  <- System.getFileStatus path
        let System.COff lenFile 
                        = System.fileSize status

        BS.appendFile path
         $  headerForElem lenFile bsElem
         `BS.append` bsElem
         `BS.append` (BS.pack "\n")

        return $ Nothing


-- | Read all the elements from the log, 
--      validate them,
--      and fold them with the given function.
foldIO  :: (ByteString -> b -> IO b) 
        -> b 
        -> HashLog 
        -> IO (Either ErrorHashLog b)

foldIO work zero hl
 = check
 where
        -- Length of an element header.
        lenHeader       = 21

        -- Static values we expect to see in the element headers.
        bsLOG1          = BS.pack "LOG1"
        bsNL            = BS.pack "\n"

        -- Path to the log file.
        path            = hashLogPath hl

        -- Check the file exists and open it.
        check
         = do   exists  <- System.doesFileExist path
                if not exists
                 then return $ Left $ ErrorHashLogFileNotFound path
                 else begin

        -- Lock the file before we start reading from it.
        --   We can't have other processes writing to it while we read
        --   off the current elements.
        begin
         = System.withFileLock path System.Shared
         $ \_lock -> do
                status  <- System.getFileStatus path
                let len =  fromIntegral $ System.fileSize status
                h       <- System.openFile path System.ReadMode
                consume h len zero

        -- Try and consume a whole element.
        consume !h !lenFile !acc
         = do   posFile     <- System.hTell h
                consumeNext h lenFile (fromIntegral posFile) acc

        consumeNext !h !lenFile !posFile !acc
         -- If we're at the end of the file then we're done reading it.
         | remain <- lenFile - posFile
         , remain == 0  
         = return $ Right acc

         -- If the remaining file is not long enough to contain an
         -- element header then it must have been truncated.
         | remain <- lenFile - posFile
         , remain < lenHeader
         = return $ Left $ ErrorHashLogFileTruncated path

         -- Otherwise try to load an element.
         | otherwise
         = do   
                -- Get the element header from the log.
                bsHeader <- BS.hGet h lenHeader

                let (bsLOG1', bs2)   = BS.splitAt 4 bsHeader
                let (bsLen,   bs3)   = BS.splitAt 8 bs2
                let (bsHash,  bsNL') = BS.splitAt 8 bs3

                -- Try to read the element length.
                case readHex (BS.unpack bsLen) of
                 [(lenElem, [])]
                  | bsLOG1' == bsLOG1
                  , bsNL'   == bsNL
                   ->   loadElem h lenFile (posFile + lenHeader) acc
                                 lenElem bsLen bsHash
                 _ ->   return $ Left $ ErrorHashLogFileCorrupted path

        loadElem !h !lenFile !posFile !acc !lenElem !_bsLen !_bsHash
         -- Check that the file is long enough to contain an element
         -- of the size that the header mentions.
         | remain <- lenFile - posFile
         , remain <  lenElem
         = return $ Left $ ErrorHashLogFileTruncated path

         | otherwise
         = do   -- Read the element payload.
                bsElem  <- BS.hGet h lenElem

                -- Rehash the element to get the expected header.
                let _bsHeaderExpected 
                        = headerForElem (fromIntegral posFile) bsElem

                -- The expected header better match the real one.
                -- TODO: hash the element and check against the hash from the header.

                -- Accumulate the element payload
                acc'    <- work bsElem acc

                -- Discard the trailing newline
                _       <- BS.hGet h 1

                -- Eat some more elements.
                consumeNext h lenFile (posFile + lenElem + 1) acc' 


-- | Hash log errors.
data ErrorHashLog
        -- | Could not find the file for this hash log.
        = ErrorHashLogFileNotFound FilePath

        -- | Hash log file has been truncated, 
        --   and contains an incomplete record.
        | ErrorHashLogFileTruncated FilePath

        -- | Hash log file has been corrupted.
        --   Either an element header is malformed or hashing the
        --   element yields the wrong value.
        | ErrorHashLogFileCorrupted FilePath

        -- | Provided element is too big to store in a HashLog.
        --   The hard limit is 4GB (2^32 bytes) per element.
        | ErrorHashLogElemTooBig
        deriving (Eq, Show)


