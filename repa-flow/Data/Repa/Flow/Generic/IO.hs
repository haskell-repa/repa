
module Data.Repa.Flow.Generic.IO
        ( -- * Sourcing
          fromFiles
        , sourceBytes
        , sourceChars
        , sourceChunks
        , sourceRecords

          -- * Sinking
        , toFiles
        , sinkBytes
        , sinkChars
        , sinkLines)
where
import Data.Repa.Flow.IO.Bucket
import Data.Repa.Flow.Generic.Operator          as F
import Data.Repa.Flow.Generic.Base              as F
import Data.Repa.Fusion.Unpack                  as F
import Data.Repa.Array.Material                 as A
import Data.Repa.Array                          as A
import Data.Char
import System.IO
import Data.Word
import Prelude                                  as P
#include "repa-flow.h"


-- fromFiles --------------------------------------------------------------------------------------
-- | Open some files as buckets and use them as `Sources`.
--
--   Finalisers are attached to the `Sources` so that each file will be 
--   closed the first time the consumer tries to an element from the associated
--   stream when no more are available.
--
---
--   TODO: reinstate finalisers
fromFiles 
        ::  (Bulk l FilePath, Target l Bucket)
        =>  Array l FilePath                    -- ^ Files to open.
        -> (Array l Bucket -> IO (Sources (Index l) IO a))  
                                                -- ^ Consumer.
        -> IO (Sources (Index l) IO a)

fromFiles paths use
 = do   
         -- Open all the files, ending up with a list of buckets.
        bs             <- mapM (flip openBucket ReadMode) $ A.toList paths

        -- Pack buckets back into an array with the same layout as
        -- the original.
        let Just bsArr =  A.fromListInto (A.layout paths) bs

        use bsArr
{-# NOINLINE fromFiles #-}


-- Sourcing ---------------------------------------------------------------------------------------
-- | Read complete records of data form a bucket, into chunks of the given length.
--   We read as many complete records as will fit into each chunk.
--
--   The records are separated by a special terminating character, which the 
--   given predicate detects. After reading a chunk of data we seek the bucket to 
--   just after the last complete record that was read, so we can continue to
--   read more complete records next time. 
--
--   If we cannot fit at least one complete record in the chunk then perform
--   the given failure action. Limiting the chunk length guards against the
--   case where a large input file is malformed, as we won't try to read the
--   whole file into memory.
-- 
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * The provided file handle must support seeking, else you'll get an exception.
--
sourceRecords 
        :: BulkI l Bucket
        => Integer              -- ^ Chunk length in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> Array l Bucket       -- ^ Source buckets.
        -> IO (Sources Int IO (Array N (Array F Word8)))

sourceRecords len pSep aFail hs
 =   smap_i (\_ !c -> A.segmentOn pSep c)
 =<< sourceChunks len pSep aFail hs
{-# INLINE sourceRecords #-}


-- | Like `sourceRecords`, but produce all records in a single vector.
sourceChunks
        :: BulkI l Bucket
        => Integer              -- ^ Chunk length in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> Array l Bucket       -- ^ Source buckets.
        -> IO (Sources Int IO (Array F Word8))

sourceChunks len pSep aFail bs
 = return $ Sources (A.length bs) pull_sourceChunks
 where  
        pull_sourceChunks i eat eject
         = let b = bs `index` i
           in  bAtEnd b >>= \eof ->
            if eof
                -- We're at the end of the file.
                then eject

            else do
                -- Read a new chunk from the file.
                arr      <- bGetArray b len

                -- Find the end of the last record in the file.
                let !mLenSlack  = findIndex pSep (A.reverse arr)

                case mLenSlack of
                 -- If we couldn't find the end of record then apply the failure action.
                 Nothing        -> aFail

                 -- Work out how long the record is.
                 Just lenSlack
                  -> do let !lenArr     = A.length arr
                        let !ixSplit    = lenArr - lenSlack

                        -- Seek the file to just after the last complete record.
                        bSeek b RelativeSeek (fromIntegral $ negate lenSlack)

                        -- Eat complete records at the start of the chunk.
                        eat $ window 0 ixSplit arr
        {-# INLINE pull_sourceChunks #-}
{-# INLINE_FLOW sourceChunks #-}


-- | Read 8-byte ASCII characters from some files, using the given chunk length.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * All chunks have the same size, except possibly the last one.
----
sourceChars 
        :: BulkI l Bucket
        => Integer              -- ^ Chunk length in bytes.
        -> Array l Bucket       -- ^ Buckets.
        -> IO (Sources Int IO (Array F Char))
sourceChars len bs
 =   smap_i (\_ !c -> A.computeS F $ A.map (chr . fromIntegral) c)
 =<< sourceBytes len bs
{-# INLINE sourceChars #-}


-- | Read data from some files, using the given chunk length.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * All chunks have the same size, except possibly the last one.
--
sourceBytes 
        :: BulkI l Bucket
        => Integer              -- ^ Chunk length in bytes.
        -> Array l Bucket       -- ^ Buckets.
        -> IO (Sources Int IO (Array F Word8))
sourceBytes len bs
 = return $ Sources (A.length bs) pull_sourceBytes
 where
        pull_sourceBytes i eat eject
         = do let b  = A.index bs i
              op  <- bIsOpen b
              if not op 
                then eject
                else do
                  eof <- bAtEnd b
                  if eof
                   then eject
                   else do
                        !chunk  <- bGetArray b len
                        eat chunk
        {-# INLINE pull_sourceBytes #-}
{-# INLINE_FLOW sourceBytes #-}


-- Sinking ----------------------------------------------------------------------------------------
-- | Open from files for writing and use the handles to create `Sinks`.
--
--   Finalisers are attached to the sinks so that file assocated with
--   each stream is closed when that stream is ejected.
--
toFiles :: (Bulk l FilePath, Target l Bucket)
        =>  Array l FilePath            -- ^ File paths.
        -> (Array l Bucket -> IO (Sinks (Index l) IO a))
                                        -- ^ Consumer.
        -> IO (Sinks (Index l) IO a)

toFiles paths use
 = do   -- Open all the files, ending up with a list of buckets.
        bs             <- mapM (flip openBucket WriteMode) $ A.toList paths

        -- Pack buckets back into an array with the same layout as
        -- the original.
        let Just bsArr =  A.fromListInto (A.layout paths) bs

        use bsArr
{-# NOINLINE toFiles #-}


-- | Write vectors of text lines to the given files handles.
-- 
--   * Data is copied into a new buffer to insert newlines before being
--     written out.
--
sinkLines 
        :: ( Bulk  l Bucket
           , BulkI l1 (Array l2 Char)
           , BulkI l2 Char, Unpack (Array l2 Char) t2)
        => Name  l1             -- ^ Layout of chunks of lines.
        -> Name  l2             -- ^ Layout of lines.
        -> Array l Bucket       -- ^ Buckets.
        -> IO (Sinks (Index l) IO (Array l1 (Array l2 Char)))
sinkLines _ _ !bs
 =   smap_o (\_ !c -> computeS F $ A.map (fromIntegral . ord) $ concatWith F fl c)
 =<< sinkBytes bs
 where  !fl     = A.fromList F ['\n']
{-# INLINE sinkLines #-}


-- | Write chunks of 8-byte ASCII characters to the given file handles.
-- 
--   * Data is copied into a foreign buffer to truncate the characters
--     to 8-bits each before being written out.
--
sinkChars 
        :: (Bulk  l Bucket, BulkI r Char)
        =>  Array l Bucket      -- ^ Buckets.
        -> IO (Sinks (Index l) IO (Array r Char))
sinkChars !bs
 =   smap_o (\_ !c -> computeS F $ A.map (fromIntegral . ord) c)
 =<< sinkBytes bs
{-# INLINE sinkChars #-}


-- | Write chunks of bytes to the given file handles.
--
--   * Data is written out directly from the provided buffer.
--
sinkBytes 
        :: Bulk  l Bucket
        => Array l Bucket       -- ^ Buckets.
        -> IO (Sinks (Index l) IO (Array F Word8))
sinkBytes !bs
 = do   let push_sinkBytes i !chunk
             = bPutArray (bs `index` i) chunk
            {-# NOINLINE push_sinkBytes #-}

        let eject_sinkBytes i
             = bClose    (bs `index` i)
            {-# INLINE eject_sinkBytes #-}

        return  $ Sinks (A.extent $ A.layout bs) push_sinkBytes eject_sinkBytes
{-# INLINE_FLOW sinkBytes #-}

