
module Data.Repa.Flow.Generic.IO
        ( fromFiles
        , toFiles

          -- * Sourcing Records
        , sourceRecords,   hSourceRecords

          -- * Sourcing Chunks
        , sourceChunks,    hSourceChunks

          -- * Sourcing Bytes
        , sourceBytes,     hSourceBytes

          -- * Sinking Bytes
        , sinkBytes,       hSinkBytes)
where
import Data.Repa.Flow.Generic.Operator
import Data.Repa.Flow.Generic.Base
import Data.Repa.Array.Unsafe.Nested                    as UN
import Data.Repa.Array.Foreign                          as R
import Data.Repa.Array                                  as R
import Data.Repa.IO.Array
import System.IO
import Data.Word
import Prelude                                          as P


lix :: [a] -> Int -> Maybe a
lix (x : _)  0  = Just x
lix (_ : xs) n  = lix xs (n - 1)
lix _        _  = Nothing

-- Reading ---------------------------------------------------------------------------------------
-- | Open some files for reading and use the handles to create `Sources`.
fromFiles 
        :: [FilePath] 
        -> ([Handle] -> IO (Sources Int IO a))
        -> IO (Sources Int IO a)

fromFiles paths use
 = do   hs      <- mapM (flip openBinaryFile ReadMode) paths
        i0      <- use hs
        finalize_i (\(IIx i _) -> let Just h = lix hs i
                                  in  hClose h) i0
{-# NOINLINE fromFiles #-}


-- | Open from files for writing and use the handles to create `Sinks`.
toFiles :: [FilePath] 
        -> ([Handle] -> IO (Sinks Int IO a))
        -> IO (Sinks Int IO a)

toFiles paths use
 = do   hs      <- mapM (flip openBinaryFile WriteMode) paths
        o0      <- use hs
        finalize_o (\(IIx i _) -> hClose (hs !! i)) o0
{-# NOINLINE toFiles #-}


-- Source Records ---------------------------------------------------------------------------------
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
--   Each file will be closed the first time the consumer tries to pull an element
--   from the associated stream when no more are available.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * The provided file handle must support seeking, else you'll get an exception.
--
sourceRecords 
        :: [FilePath]           -- ^ File paths.
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a
                                --   whole record.
        -> IO (Sources Int IO (Vector UN (Vector F Word8)))

sourceRecords filePaths len pSep aFail
 = do   hs      <- mapM (flip openBinaryFile ReadMode) filePaths
        s0      <- hSourceRecords hs len pSep aFail
        finalize_i (\(IIx i _) -> let Just h = lix hs i
                                  in  hClose h) s0
{-# NOINLINE sourceRecords #-}
--  NOINLINE because the chunks should be big enough to not require fusion,
--           and we don't want to release the code for 'openBinaryFile'.


-- | Like `sourceRecords`, but taking an existing file handle.
hSourceRecords 
        :: [Handle]             -- File handles.
        -> Int                  -- Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- Detect the end of a record.        
        -> IO ()                -- Action to perform if we can't get a whole record.
        -> IO (Sources Int IO (Vector UN (Vector F Word8)))

hSourceRecords hs len pSep aFail
 =   smap_i (\_ !c -> UN.segmentOn pSep c)
 =<< hSourceChunks hs len pSep aFail
{-# INLINE [2] hSourceRecords #-}


-- Source Chunks ----------------------------------------------------------------------------------
-- | Like `sourcesRecords`, but produce all records in a single vector.
sourceChunks
        :: [FilePath]           -- ^ File paths.
        -> Int                  -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> IO (Sources Int IO (Vector F Word8))

sourceChunks filePaths len pSep aFail
 = do   hs      <- mapM (flip openBinaryFile ReadMode) filePaths
        s0      <- hSourceChunks hs len pSep aFail
        finalize_i (\(IIx i _) -> let Just h = lix hs i
                                  in  hClose h) s0
{-# NOINLINE sourceChunks #-}
--  NOINLINE because the chunks should be big enough to not require fusion,
--           and we don't want to release the code for 'openBinaryFile'.


-- | Like `hSourceRecords`, but produce all records in a single vector.
hSourceChunks
        :: [Handle]             -- File handles.
        -> Int                  -- Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- Detect the end of a record.        
        -> IO ()                -- Action to perform if we can't get a whole record.
        -> IO (Sources Int IO (Vector F Word8))

hSourceChunks hs len pSep aFail
 = return $ Sources (P.length hs) pull_hSourceChunks
 where  
        pull_hSourceChunks (IIx i _) eat eject
         = let Just h = lix hs i
           in hIsEOF h >>= \eof ->
            if eof
                -- We're at the end of the file.
                then eject

            else do
                -- Read a new chunk from the file.
                arr      <- hGetArray (hs !! i) len

                -- Find the end of the last record in the file.
                let !mLenSlack  = findIndex pSep (R.reverse arr)

                case mLenSlack of
                 -- If we couldn't find the end of record then apply the failure action.
                 Nothing        -> aFail

                 -- Work out how long the record is.
                 Just lenSlack
                  -> do let !lenArr     = size (extent arr)
                        let !ixSplit    = lenArr - lenSlack

                        -- Seek the file to just after the last complete record.
                        hSeek (hs !! i) RelativeSeek (fromIntegral $ negate lenSlack)

                        -- Eat complete records at the start of the chunk.
                        eat $ window (Z :. 0) (Z :. ixSplit) arr
        {-# INLINE pull_hSourceChunks #-}
{-# INLINE [2] hSourceChunks #-}


-- Source Bytes -----------------------------------------------------------------------------------
-- | Read data from some files, using the given chunk length.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * All chunks have the same size, except possibly the last one.
--
--   Each file will be closed the first time the consumer tries to pull an element
--   from the associated stream when no more are available.
--
sourceBytes 
        :: [FilePath] -> Int 
        -> IO (Sources Int IO (Vector F Word8))

sourceBytes filePaths len
 = do   hs      <- mapM (flip openBinaryFile ReadMode) filePaths
        s0      <- hSourceBytes hs len
        finalize_i (\(IIx i _) -> let Just h = lix hs i
                                  in  hClose h) s0
{-# NOINLINE sourceBytes #-}
--  NOINLINE because the chunks should be big enough to not require fusion,
--           and we don't want to release the code for 'openBinaryFile'.


-- | Like `fileSourceBytes`, but taking existing file handles.
hSourceBytes 
        :: [Handle] -> Int 
        -> IO (Sources Int IO (Vector F Word8))

hSourceBytes hs len
 = return $ Sources (P.length hs) pull_hSource
 where
        pull_hSource (IIx i _) eat eject
         = do print i
              let Just h  = lix hs i
              eof <- hIsEOF h
              if eof 
                  then  eject
                  else do  
                        !chunk  <- hGetArray h len
                        eat chunk
        {-# INLINE pull_hSource #-}
{-# INLINE [2] hSourceBytes #-}


-- Sink Bytes -------------------------------------------------------------------------------------
-- | Write chunks of data to the given files.
--
--   Each file will be closed when the associated stream is ejected.
--
sinkBytes :: [FilePath] -> IO (Sinks Int IO (Vector F Word8))
sinkBytes filePaths
 = do   hs      <- mapM (flip openBinaryFile WriteMode) filePaths
        o0      <- hSinkBytes hs
        finalize_o (\(IIx i _) -> hClose (hs !! i)) o0
{-# NOINLINE sinkBytes #-}
--  NOINLINE because the chunks should be big enough to not require fusion,
--           and we don't want to release the code for 'openBinaryFile'.


-- | Write chunks of data to the given file handles.
hSinkBytes :: [Handle] -> IO (Sinks Int IO (Vector F Word8))
hSinkBytes !hs
 = do   let push_hSinkBytes (IIx i _) !chunk
                = hPutArray (hs !! i) chunk
            {-# NOINLINE push_hSinkBytes #-}

        let eject_hSinkBytes _
                = return ()
            {-# INLINE eject_hSinkBytes #-}

        return  $ Sinks (P.length hs) push_hSinkBytes eject_hSinkBytes
{-# INLINE [2] hSinkBytes #-}

