
module Data.Repa.Flow.IO.Bucket
        ( Bucket
        , bucketLength
        , openBucket

          -- * Reading files
        , bucketsFromFile

          -- * Bucket IO
        , bClose
        , bAtEnd
        , bGetArray)
where
import Data.Word
import System.IO
import Control.Monad
import Data.Repa.Array                  as A
import Data.Repa.Array.Material         as A
import Data.Repa.IO.Array               as A
import qualified Foreign.Storable       as Foreign
import qualified Foreign.Marshal.Alloc  as Foreign
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
        { -- | Physical location of the file.
          bucketFilePath        :: FilePath 

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
        return  $ Bucket
                { bucketFilePath        = path
                , bucketStartPos        = 0
                , bucketLength          = Nothing
                , bucketHandle          = h }


-- | Open a file containing atomic records and split it into the given number
--   of evenly sized buckets. 
--
--   The records separated by a special terminating charater, which the
--   given predicate detects. The file is split cleanly on record boundaries, 
--   so we get a whole number of records in each bucket. As the records can be
--   of varying size, the buckets are not guaranteed to have exactly the same
--   length, in either records or buckets, though we try to give them the
--   approximatly the same number of bytes.
--
--   * The provided file handle must support seeking, else you'll get an
--     exception.
--
bucketsFromFile 
        :: FilePath             -- ^ Path to file to read.
        -> Int                  -- ^ Number of buckets.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.
        -> IO [Bucket]          -- ^ Produced buckets.

bucketsFromFile path n pEnd 
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

        starts    <- loop_advances lenTotal 0 hh

        -- Ending positions and lengths for each bucket.
        let ends  = tail (starts ++ [lenTotal])
        let lens  = P.map (\(start, end) -> end - start) 
                  $ P.zip starts ends

        return [ Bucket
                  { bucketFilePath = path
                  , bucketStartPos = start
                  , bucketLength   = Just len
                  , bucketHandle   = h }       
                        | start <- starts
                        | len   <- lens
                        | h     <- hh ]
{-# NOINLINE bucketsFromFile #-}
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


-- Bucket IO ------------------------------------------------------------------

-- | Close a bucket, releasing the contained filed handle.
bClose :: Bucket -> IO ()
bClose bucket
        = hClose $ bucketHandle bucket
{-# NOINLINE bClose #-}


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


-- | Get some data from a bucket.
bGetArray :: Bucket -> Integer -> IO (Array F Word8)
bGetArray bucket lenWanted
 = do   
        let len = case bucketLength bucket of
                        Nothing         -> lenWanted
                        Just lenMax     -> max len lenMax

        hGetArray (bucketHandle bucket) 
         $ fromIntegral len
{-# NOINLINE bGetArray #-}





