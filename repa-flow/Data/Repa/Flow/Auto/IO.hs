
-- | Read and write files.
--
--   The functions in this module are wrappers for the ones in 
--   "Data.Repa.Flow.Default.SizedIO" that use a default chunk size of
--   64kBytes and just call `error` if the source file appears corruped. 
--
module Data.Repa.Flow.Auto.IO
        ( defaultChunkSize

          -- * Buckets
        , module Data.Repa.Flow.IO.Bucket

          -- * Sourcing
        , sourceBytes
        , sourceChars
        , sourceLines
        , sourceRecords
        , sourceTSV
        , sourceCSV
        , sourceFormatLn

          -- * Sinking
        , sinkBytes
        , sinkLines
        , sinkChars
        , sinkFormatLn
        )
where
import Data.Repa.Flow.Auto
import Data.Repa.Flow.IO.Bucket
import Data.Repa.Array.Material                 as A
import Data.Repa.Array.Auto.Format              as A
import Data.Repa.Array.Generic                  as A
import System.IO
import Data.Word
import qualified Data.Repa.Flow.Generic         as G
import qualified Data.Repa.Flow.Generic.IO      as G
import qualified Data.Repa.Flow.Auto.SizedIO    as F
import Prelude                                  as P
#include "repa-flow.h"


-- | The default chunk size of 64kBytes.
defaultChunkSize :: Integer
defaultChunkSize = 64 * 1024


---------------------------------------------------------------------------------------------------
-- | Read data from some files, using the given chunk length.
sourceBytes :: Array B Bucket -> IO (Sources Word8)
sourceBytes = F.sourceBytes defaultChunkSize
{-# INLINE sourceBytes #-}


-- | Read 8-bit ASCII characters from some files, using the given chunk length.
sourceChars :: Array B Bucket -> IO (Sources Char)
sourceChars = F.sourceChars defaultChunkSize
{-# INLINE sourceChars #-}


-- | Read complete records of data form a file, into chunks of the given length.
--   We read as many complete records as will fit into each chunk.
--
--   The records are separated by a special terminating character, which the 
--   given predicate detects. After reading a chunk of data we seek the file to 
--   just after the last complete record that was read, so we can continue to
--   read more complete records next time. 
--
--   If we cannot fit at least one complete record in the chunk then perform
--   the given failure action. Limiting the chunk length guards against the
--   case where a large input file is malformed, as we won't try to read the
--   whole file into memory.
-- 
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * The provided file handle must support seeking, else you'll get an
--     exception.
--   * Each file is closed the first time the consumer tries to pull a
--     record from the associated stream when no more are available.
--
sourceRecords 
        :: (Word8 -> Bool)      -- ^ Detect the end of a record.
        -> Array B Bucket       -- ^ Input Buckets.
        -> IO (Sources (Array A Word8))
sourceRecords pSep 
        = F.sourceRecords defaultChunkSize pSep
        $ error $  "Record exceeds chunk size of " 
                ++ show defaultChunkSize ++ "bytes."
{-# INLINE sourceRecords #-}


-- | Read complete lines of data from a text file, using the given chunk length.
--   We read as many complete lines as will fit into each chunk.
--
--   * The trailing new-line characters are discarded.
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * The provided file handle must support seeking, else you'll get an
--     exception.
--   * Each file is closed the first time the consumer tries to pull a line
--     from the associated stream when no more are available.
--
sourceLines 
        :: Array B Bucket -> IO (Sources (Array A Char))
sourceLines     
        = F.sourceLines   defaultChunkSize
        $ error $  "Line exceeds chunk size of "
                ++ show defaultChunkSize ++ "bytes."
{-# INLINE sourceLines #-}


-- | Read a file containing Comma-Separated-Values.
sourceCSV :: Array B Bucket 
          -> IO (Sources (Array A (Array A Char)))
sourceCSV
        = F.sourceCSV defaultChunkSize
        $ error $  "Line exceeds chunk size of "
                ++ show defaultChunkSize ++ "bytes."
{-# INLINE sourceCSV #-}


-- | Read a file containing Tab-Separated-Values.
sourceTSV :: Array B Bucket 
          -> IO (Sources (Array A (Array A Char)))
sourceTSV
        = F.sourceTSV defaultChunkSize
        $ error $  "Line exceeds chunk size of "
                ++ show defaultChunkSize ++ "bytes."
{-# INLINE sourceTSV #-}


-- | Read the lines of a text file,
--   converting each line to values with the given format.
sourceFormatLn
        :: ( Packable format
           , Target A (Value format))
        => Integer                      -- ^ Chunk length.
        -> IO ()                        -- ^ Action when a line is too long.
        -> IO (Array A Word8 -> IO ())  -- ^ Action if we can't convert a value.
        -> format                       -- ^ Format of each line.
        -> Array B Bucket               -- ^ Source buckets.
        -> IO (Sources (Value format))

sourceFormatLn = G.sourceLinesFormat
{-# INLINE sourceFormatLn #-}


---------------------------------------------------------------------------------------------------
-- | Write 8-bit bytes to some files.
sinkBytes :: Array B Bucket -> IO (Sinks Word8)
sinkBytes bs
        =   G.map_o (A.convert F)
        =<< G.sinkBytes bs
{-# INLINE sinkBytes #-}


-- | Write 8-bit ASCII characters to some files.
sinkChars :: Array B Bucket -> IO (Sinks Char)
sinkChars =  G.sinkChars
{-# INLINE sinkChars #-}


-- | Write vectors of text lines to the given files handles.
sinkLines :: Array B Bucket -> IO (Sinks (Array A Char))
sinkLines = G.sinkLines A A
{-# INLINE sinkLines #-}


-- | Create sinks that convert values to some format and writes
--   them to buckets.
--
-- @
-- > import Data.Repa.Flow           as F
-- > import Data.Repa.Convert.Format as F
-- > :{
--   do let format = FixString ASCII 10 :*: Float64be :*: Int16be
--      let vals   = listFormat format
--                    [ \"red\"   :*: 5.3    :*: 100
--                    , \"green\" :*: 2.8    :*: 93 
--                    , \"blue\"  :*: 0.99   :*: 42 ]
--
--      ss  <- F.fromList 1 vals
--      out <- toFiles' [\"colors.bin\"] 
--          $  sinkFormatLn format (error \"convert failed\")
--      drainS ss out
--   :}
-- @
--
sinkFormatLn
        :: (Packable format, Bulk A (Value format), Show format)
        => format                       -- ^ Binary format for each value.
        -> IO ()                        -- ^ Action when a value cannot be serialized.
        -> Array B Bucket               -- ^ Output buckets.
        -> IO (Sinks (Value format))

sinkFormatLn format aFail bs
 = return $ G.Sinks (A.length bs) 
                    push_sinkFormatLn eject_sinkFormatLn
 where  
        push_sinkFormatLn i !chunk
         = case A.packsFormatLn format chunk of
                Nothing   -> aFail
                Just buf  -> bPutArray (bs `index` i) (A.convert F buf)
        {-# INLINE push_sinkFormatLn #-}

        eject_sinkFormatLn i 
         = bClose (bs `index` i)
        {-# INLINE eject_sinkFormatLn #-}
{-# INLINE_FLOW sinkFormatLn #-}


---------------------------------------------------------------------------------------------------
{-
-- | Create sinks that write values from some binary Repa table,
--   where all the values have a fixed length.
toTable :: (Packable format, Bulk A (Value format))
        => FilePath             -- ^ Directory holding table.
        -> Int                  -- ^ Number of buckets to use.
        -> format               -- ^ Format of data.
        -> IO ()                -- ^ Action when a value cannot be serialised.
        -> IO (Maybe (Sinks (Value format)))

toTable path nBuckets format aFail
 | nBuckets <= 0
 = return $ Nothing

 | otherwise
 = do   
        createDirectory path

        -- Create all the bucket files.
        let makeName i  = path </> ((replicate (6 - (P.length $ show i)) '0') ++ show i)
        let names       = [makeName i | i <- [0 .. nBuckets - 1]]
        let newBucket file
             = do h  <- openBinaryFile file WriteMode
                  return $ Bucket
                         { bucketFilePath       = Just file 
                         , bucketStartPos       = 0
                         , bucketLength         = Nothing
                         , bucketHandle         = h }

        bs <- mapM newBucket names

        -- Create a sink bundle for the buckets.
        kk <- sinkFormatLn format aFail (A.fromList B bs)
        return $ Just kk
{-# INLINE_FLOW toTable #-}


-- | Create sources that read values from some binary Repa table,
--   where all the values have a fixed length.
fromTable
        :: (Packable format, Target A (Value format))
        => FilePath             -- ^ Directory holding table.
        -> format               -- ^ Format of data.
        -> IO ()                -- ^ Action when a value cannot be deserialised.
        -> IO (Maybe (Sources (Value format)))

fromTable path format aFail
 = do
        -- All the files in the table directory.
        fs      <- getDirectoryContents path

        -- Filter out special file names and make them relative to the dir stem.
        let fsRel
                = P.map (path </>)
                $ P.filter (\f -> f /= "." && f /= "..") fs

        let newBucket file
             = do h <- openBinaryFile file ReadMode
                  return $ Bucket
                         { bucketFilePath       = Just file
                         , bucketStartPos       = 0
                         , bucketLength         = Nothing
                         , bucketHandle         = h }

        bs <- mapM newBucket fsRel

        -- Create a source bundle for the buckets.
        ss <- sourceFixedFormat format aFail (A.fromList B bs)
        return $ Just ss
{-# INLINE_FLOW fromTable #-}
-}
