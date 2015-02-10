
module Data.Repa.Flow.IO.Bucket
        ( Bucket
        , bucketLength
        , openBucket
        , hBucket

          -- * Reading
        , bucketsFromFile
        , bucketsFromFileAt

          -- * Writing 
        , bucketsToDir
        , bucketsToDirs
        , bucketToFile

          -- * Bucket IO
        , bClose
        , bIsOpen
        , bAtEnd
        , bSeek
        , bGetArray
        , bPutArray)
where
import Data.Repa.Array                  as A
import Data.Repa.Array.Material         as A
import Data.Repa.Array.RowWise          as A
import Data.Repa.IO.Array               as A
import qualified Foreign.Storable       as Foreign
import qualified Foreign.Marshal.Alloc  as Foreign
import Control.Monad
import Data.Word
import System.IO
import System.FilePath
import System.Directory
import Prelude                          as P


-- | A bucket represents portion of a whole data-set on disk,
--   and contains a file handle that points to the next piece of 
--   data to be read or written.
--  
--   The bucket could be created from a portion of a single flat file,
--   or be one file of a split data set. The main advantage over a plain
--  `Handle` is that a `Bucket` can represent a small portion of a larger
--   file.
--
data Bucket
        = Bucket
        { -- | Physical location of the file, if known.
          bucketFilePath        :: Maybe FilePath 

          -- | Starting position of the bucket in the file, in bytes.
        , bucketStartPos        :: Integer

          -- | Maximum length of the bucket, in bytes.
          --
          --   If `Nothing` then the length is indeterminate, which is used
          --   when writing to files.
        , bucketLength          :: Maybe Integer

          -- | File handle for the bucket.
          -- 
          --   If several buckets have been created from a single file,
          --   then all buckets will have handles bound to that file,
          --   but they will be at different positions.
        , bucketHandle          :: Handle }


-- | Open a file as a single bucket.
openBucket :: FilePath -> IOMode -> IO Bucket
openBucket path mode
 = do   h       <- openBinaryFile path mode
        hSeek h SeekFromEnd  0
        lenTotal <- hTell h
        hSeek h AbsoluteSeek 0 

        return  $ Bucket
                { bucketFilePath        = Just path
                , bucketStartPos        = 0
                , bucketLength          = Just lenTotal
                , bucketHandle          = h }
{-# NOINLINE openBucket #-}


-- | Wrap an existing file handle as a bucket.
hBucket :: Handle -> IO Bucket
hBucket h
 = do   pos     <- hTell h
        return  $ Bucket
                { bucketFilePath        = Nothing
                , bucketStartPos        = pos
                , bucketLength          = Nothing
                , bucketHandle          = h }
{-# NOINLINE hBucket #-}


-- From Files -----------------------------------------------------------------
-- | Open a file containing atomic records and split it into the given number
--   of evenly sized buckets. 
--
--   The records are separated by a special terminating charater, which the
--   given predicate detects. The file is split cleanly on record boundaries, 
--   so we get a whole number of records in each bucket. As the records can be
--   of varying size the buckets are not guaranteed to have exactly the same
--   length, in either records or buckets, though we try to give them the
--   approximatly the same number of bytes.
--
bucketsFromFile
        :: Int                          -- ^ Number of buckets.
        -> (Word8 -> Bool)              -- ^ Detect the end of a record.
        -> FilePath                     -- ^ File to open.
        -> (Array B Bucket -> IO b)     -- ^ Consumer.
        -> IO b

bucketsFromFile n pEnd path use
        = bucketsFromFileAt n pEnd path 0 use
{-# INLINE bucketsFromFile #-}


-- | Like `bucketsFromFile` but start at the given offset.
bucketsFromFileAt
        :: Int                          -- ^ Number of buckets.
        -> (Word8 -> Bool)              -- ^ Detect the end of a record.
        -> FilePath                     -- ^ File to open.
        -> Integer                      -- ^ Starting offset.
        -> (Array B Bucket -> IO b)     -- ^ Consumer.
        -> IO b

bucketsFromFileAt n pEnd path offsetStart use
 = do
        -- Open the file first to check its length.
        h0       <- openBinaryFile path ReadMode
        hSeek h0 SeekFromEnd 0
        lenTotal <- hTell h0
        hClose h0

        -- Open a file handle for each of the buckets.
        -- The handles start at the begining of the file and still need
        -- to be advanced.
        hh      <- mapM (flip openBinaryFile ReadMode) (replicate n path)

        -- Advance all the handles to the start of their part of the file.
        let loop_advances _      _    [] 
             = return []

            loop_advances _      pos1 (_h1 : [])
             = return [pos1]

            loop_advances remain pos1 (h1 : h2 : hs)
             = do 
                  -- Push the next handle an even distance into the
                  -- remaining part of the file.
                  let lenWanted 
                       = remain `div` (fromIntegral $ P.length (h1 : h2 : hs))
                  let posWanted = pos1 + lenWanted
                  hSeek h2 AbsoluteSeek posWanted

                  -- Now advance it until we get to the end of a record.
                  pos2          <- advance h2 pEnd 
                  let remain'   = lenTotal - pos2

                  poss          <- loop_advances remain' pos2 (h2 : hs)
                  return $ pos1 : poss

        starts    <- loop_advances lenTotal offsetStart hh

        -- Ending positions and lengths for each bucket.
        let ends  = tail (starts ++ [lenTotal])
        let lens  = P.map (\(start, end) -> end - start) 
                  $ P.zip starts ends

        let bs    = [ Bucket
                        { bucketFilePath = Just path
                        , bucketStartPos = start
                        , bucketLength   = Just len
                        , bucketHandle   = h }       
                              | start <- starts
                              | len   <- lens
                              | h     <- hh ]

        use  $ A.fromList B bs
{-# NOINLINE bucketsFromFileAt #-}
--  NOINLINE to avoid polluting the core code of the consumer.
--  This prevents it from being specialised for the pEnd predicate, 
--  but we're expecting pEnd to be applied a small number of times,
--  so it shouldn't matter.


-- | Advance a file handle until we reach a byte that, matches the given 
--   predicate, then return the final file position.
advance :: Handle -> (Word8 -> Bool) -> IO Integer
advance h pEnd
 = do   buf     <- Foreign.mallocBytes 1

        let loop_advance 
             = do c <- hGetBuf h buf 1
                  if c == 0
                   then return ()
                   else do
                        x <- Foreign.peek buf
                        if pEnd x
                         then return ()
                         else loop_advance
        loop_advance
        Foreign.free buf
        hTell h
{-# NOINLINE advance #-}


-- To Dirs -------------------------------------------------------------------
-- | Create a new directory of the given name, containing the given number
--   of buckets. 
--
--   If the dir is named @somedir@ then the buckets are named
--   @somedir/000000@, @somedir/000001@, @somedir/000002@ and so on.
--
--   * The number of buckets must be greater than zero, else `Nothing`.
bucketsToDir 
        :: Int                          -- ^ Number of buckets to create.
        -> FilePath                     -- ^ Path to directory.
        -> (Array B Bucket -> IO b)     -- ^ Consumer.
        -> IO (Maybe b)

bucketsToDir n path use
 | n <= 0        
 = return Nothing

 | otherwise
 = do   
        createDirectory path

        let makeName i = path </> ((replicate (6 - (P.length $ show i)) '0') ++ show i)
        let names      = [makeName i | i <- [0 .. n - 1]]

        let newBucket file
             = do h      <- openBinaryFile file WriteMode
                  return $  Bucket
                         { bucketFilePath       = Just file
                         , bucketStartPos       = 0
                         , bucketLength         = Nothing
                         , bucketHandle         = h }

        bs <- mapM newBucket names
        liftM Just $ use (A.fromList B bs)
{-# NOINLINE bucketsToDir #-}


-- | Given a list of directories, create those directories and open 
--   the given number of buckets per directory.
--
--   * The number of buckets must be greater than zero, else `Nothing`.
--
bucketsToDirs 
        :: Int                  -- ^ Number of buckets to create per directory.
        -> [FilePath]           -- ^ Path to directories.
        -> (Array (E B DIM2) Bucket -> IO b)     
                                -- ^ Consumer.
        -> IO (Maybe b)

bucketsToDirs nBucketsPerDir paths use
 | nBucketsPerDir <= 0        
 = return Nothing

 | otherwise
 = do   
        mapM_ createDirectory paths

        let makeName path i 
                = path </> ((replicate (6 - (P.length $ show i)) '0') ++ show i)

        let newBucket file
             = do h      <- openBinaryFile file WriteMode
                  return $  Bucket
                         { bucketFilePath       = Just file
                         , bucketStartPos       = 0
                         , bucketLength         = Nothing
                         , bucketHandle         = h }

        let newDir path
             = do createDirectory path
                  bs    <- mapM newBucket 
                        $ [makeName path i | i <- [0 .. nBucketsPerDir - 1]]

                  return bs

        -- Make all the buckets, then pack them into a matrix.
        bs        <- liftM P.concat $ P.mapM newDir paths

        -- Pack the buckets into an array 
        let Just bsArr 
                = A.fromListInto 
                        (A.matrix B (P.length paths) nBucketsPerDir) 
                        bs

        liftM Just $ use bsArr
{-# NOINLINE bucketsToDirs #-}



-- To Files -------------------------------------------------------------------
-- | Open a file for writing as a bucket.
bucketToFile 
        :: FilePath                     -- ^ Path to bucket.
        -> (Array B Bucket -> IO b)     -- ^ Consumer
        -> IO b

bucketToFile file use
 = do   h       <- openBinaryFile file WriteMode
        let b   = Bucket
                { bucketFilePath        = Just file
                , bucketStartPos        = 0
                , bucketLength          = Nothing
                , bucketHandle          = h }

        use $ A.fromList B [b]
{-# NOINLINE bucketToFile #-}


-- Bucket IO ------------------------------------------------------------------
-- | Close a bucket, releasing the contained file handle.
bClose :: Bucket -> IO ()
bClose bucket
        = hClose $ bucketHandle bucket
{-# NOINLINE bClose #-}


-- | Check if the bucket is currently open.
bIsOpen :: Bucket -> IO Bool
bIsOpen bucket
        = hIsOpen $ bucketHandle bucket
{-# NOINLINE bIsOpen #-}


-- | Check if the contained file handle is at the end of the bucket.
bAtEnd :: Bucket -> IO Bool
bAtEnd bucket
 = do   eof     <- hIsEOF $ bucketHandle bucket

        -- Position in the file.
        posFile  <- hTell $ bucketHandle bucket

        -- Check for bogus position before we subtract the startPos.
        -- If this happenes then something has messed with our handle.
        when (posFile < bucketStartPos bucket)
         $ error "repa-flow.bAtEnd: handle position is outside bucket."

        -- Position in the bucket.
        let posBucket = posFile - bucketStartPos bucket

        return $ eof || (case bucketLength bucket of
                                Nothing  -> False
                                Just len -> posBucket >= len)
{-# NOINLINE bAtEnd #-}


-- | Seek to a position with a bucket.
bSeek :: Bucket -> SeekMode -> Integer -> IO ()
bSeek bucket mode offset
 = do   
        -- The current position in the underlying file.
        posFile <- hTell $ bucketHandle bucket

        -- Apply the seek mode to get the wanted position in the file,
        --  which might be outside the bucket.
        --  If Nothing it means the end of the file.
        let posWanted 
             = case mode of
                AbsoluteSeek     
                 -> Just $ bucketStartPos bucket + max 0 offset

                RelativeSeek     
                 -> Just $ posFile + offset

                SeekFromEnd
                 -> case bucketLength bucket of
                        Nothing  -> Nothing
                        Just len -> Just $ bucketStartPos bucket 
                                         + len - max 0 offset

        -- Clip the wanted position so that it's inside the bucket.
        let posActual
             = case posWanted of
                Nothing
                 -> Nothing

                Just wanted      
                  |  Just len    <- bucketLength bucket
                  -> if      wanted < bucketStartPos bucket
                        then Just $ bucketStartPos bucket
                     else if wanted > bucketStartPos bucket + len
                        then Just $ bucketStartPos bucket + len
                     else Just wanted

                  | otherwise
                  -> if     wanted < bucketStartPos bucket
                       then Just $ bucketStartPos bucket
                       else Just wanted

        case posActual of
         Nothing   -> hSeek (bucketHandle bucket) SeekFromEnd  0
         Just pos  -> hSeek (bucketHandle bucket) AbsoluteSeek pos
{-# NOINLINE bSeek #-}


-- | Get some data from a bucket.
bGetArray :: Bucket -> Integer -> IO (Array F Word8)
bGetArray bucket lenWanted
 = do   
        -- Curent position in the file.
        posFile         <- hTell $ bucketHandle bucket
        let posBucket   =  posFile - bucketStartPos bucket

        let len         = case bucketLength bucket of
                           Nothing         -> lenWanted
                           Just lenMax
                            -> let lenRemain = lenMax - posBucket
                               in  min lenWanted lenRemain

        hGetArray (bucketHandle bucket) 
         $ fromIntegral len
{-# NOINLINE bGetArray #-}


-- | Put some data in a bucket.
bPutArray :: Bucket -> Array F Word8 -> IO ()
bPutArray bucket arr
        = hPutArray (bucketHandle bucket) arr
{-# NOINLINE bPutArray #-}

