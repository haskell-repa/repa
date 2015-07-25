
module Data.Repa.Store.Prim.HashLog
        ( HashLog
        , connect
        , append
        , foldIO
        , ErrorHashLog (..))
where
import Data.ByteString.Char8            (ByteString)
import Data.ByteString.Base16           as B16
import qualified System.IO              as System
import qualified System.Directory       as System
import qualified System.Posix.Files     as Posix
import qualified Crypto.Hash.MD5        as MD5
import qualified Data.ByteString.Char8  as BS
import Prelude                          as P
import Numeric


-- | A hashed container format.
--
--   Contains a sequence of values, along with the size of each value and
--   and a MD5 hash, stored as a human-readable file.
--
data HashLog
        = HashLog
        { hashLogPath   :: FilePath }


-- | Connect to an external `HashLog`.
connect :: FilePath -> IO (Maybe HashLog)
connect path 
        = return $ Just HashLog
        { hashLogPath   = path }


-- | Append a new value to the log.
append :: HashLog -> ByteString -> IO (Maybe ErrorHashLog)
append hl bs
 = let !lenData =BS.length bs
   in if lenData >= 2^(32 :: Integer)
       then return $ Just ErrorHashLogElemTooBig
       else do
        -- Get the current file length.
        exists  <- System.doesFileExist (hashLogPath hl)
        lenFile <- if exists
                        then do status <- Posix.getFileStatus (hashLogPath hl)
                                return $  Posix.fileSize status
                        else return 0

        -- Length of the data.
        let strLenData   = showHex lenData ""
        let bsLenDataPad = BS.replicate (8 - P.length strLenData) '0' 
                          `BS.append` BS.pack strLenData

        -- Hash the data.
        let bsHash       = MD5.hash (bs `BS.append` BS.pack (show lenFile))
        let bsHash8      = BS.take 8 $ B16.encode bsHash

        BS.appendFile (hashLogPath hl)
         $           (BS.pack "LOG1")
         `BS.append` bsLenDataPad
         `BS.append` bsHash8
         `BS.append` (BS.pack "\n")
         `BS.append` bs
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
                 else do
                        status  <- Posix.getFileStatus path
                        let len =  fromIntegral $ Posix.fileSize status
                        h       <- System.openFile path System.ReadMode
                        consume h len zero

        -- Try and consume a whole element.
        consume !h !lenFile !acc
         = do   posFile     <- System.hTell h
                loadHead h lenFile (fromIntegral posFile) acc

        loadHead !h !lenFile !posFile !acc
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
                let (_bsHash, bsNL') = BS.splitAt 8 bs3

                -- Try to read the element length.
                case readHex (BS.unpack bsLen) of
                 [(lenElem, [])]
                  | bsLOG1' == bsLOG1
                  , bsNL'   == bsNL
                   ->   loadElem h lenFile (posFile + lenHeader) acc lenElem
                 _ ->   return $ Left $ ErrorHashLogFileCorrupted path

        loadElem !h !lenFile !posFile !acc !lenElem
         -- Check that the file is long enough to contain an element
         -- of the size that the header mentions.
         | remain <- lenFile - posFile
         , remain <  lenElem
         = return $ Left $ ErrorHashLogFileTruncated path

         | otherwise
         = do   -- Read the element payload.
                bsElem  <- BS.hGet h lenElem

                -- TODO: hash the element and check against the hash from the header.

                -- Accumulate the element payload
                acc'    <- work bsElem acc

                -- Discard the trailing newline
                _       <- BS.hGet h 1

                -- Eat some more elements.
                loadHead h lenFile (posFile + lenElem + 1) acc' 


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
        --   The hard limit is 4GB per element.
        | ErrorHashLogElemTooBig
        deriving (Eq, Show)


