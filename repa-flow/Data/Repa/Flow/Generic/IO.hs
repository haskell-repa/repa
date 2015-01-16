
module Data.Repa.Flow.Generic.IO
        ( -- * Sourcing
          fromFiles
        , sourceRecords
        , sourceChunks
        , sourceChars
        , sourceBytes

          -- * Sinking
        , toFiles
        , sinkChars
        , sinkBytes)
where
import Data.Repa.Flow.Generic.Operator
import Data.Repa.Flow.Generic.Base
import Data.Repa.Array.Unsafe.Nested                    as UN
import Data.Repa.Array.Foreign                          as A
import Data.Repa.Array                                  as A
import Data.Repa.IO.Array
import Data.Char
import System.IO
import Data.Word
import Prelude                                          as P


lix :: [a] -> Int -> Maybe a
lix (x : _)  0  = Just x
lix (_ : xs) n  = lix xs (n - 1)
lix _        _  = Nothing


-- Sourcing ---------------------------------------------------------------------------------------
-- | Open some files for reading and use the handles to create `Sources`.
--
--   Finalisers are attached to the `Sources` so that each file will be 
--   closed the first time the consumer tries to an element from the associated
--   stream when no more are available.
--
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
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * The provided file handle must support seeking, else you'll get an exception.
--
sourceRecords 
        :: Int                  -- ^ Chunk length in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> [Handle]             -- ^ File handles.
        -> IO (Sources Int IO (Vector UN (Vector F Word8)))

sourceRecords len pSep aFail hs
 =   smap_i (\_ !c -> UN.segmentOn pSep c)
 =<< sourceChunks len pSep aFail hs
{-# INLINE [2] sourceRecords #-}


-- | Like `sourceRecords`, but produce all records in a single vector.
sourceChunks
        :: Int                  -- ^ Chunk length in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a whole record.
        -> [Handle]             -- ^ File handles.
        -> IO (Sources Int IO (Vector F Word8))

sourceChunks len pSep aFail hs
 = return $ Sources (P.length hs) pull_sourceChunks
 where  
        pull_sourceChunks (IIx i _) eat eject
         = let Just h = lix hs i
           in hIsEOF h >>= \eof ->
            if eof
                -- We're at the end of the file.
                then eject

            else do
                -- Read a new chunk from the file.
                arr      <- hGetArray (hs !! i) len

                -- Find the end of the last record in the file.
                let !mLenSlack  = findIndex pSep (A.reverse arr)

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
        {-# INLINE pull_sourceChunks #-}
{-# INLINE [2] sourceChunks #-}


-- | Read 8-byte ASCII characters from some files, using the given chunk length.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * All chunks have the same size, except possibly the last one.
----
sourceChars :: Int -> [Handle] -> IO (Sources Int IO (Vector F Char))
sourceChars len hs
 =   smap_i (\_ !c -> A.computeS_ $ A.map (chr . fromIntegral) c)
 =<< sourceBytes len hs
{-# INLINE [2] sourceChars #-}


-- | Read data from some files, using the given chunk length.
--
--   * Data is read into foreign memory without copying it through the GHC heap.
--   * All chunks have the same size, except possibly the last one.
--
sourceBytes :: Int -> [Handle] -> IO (Sources Int IO (Vector F Word8))
sourceBytes len hs
 = return $ Sources (P.length hs) pull_sourceBytes
 where
        pull_sourceBytes (IIx i _) eat eject
         = do print i
              let Just h  = lix hs i
              eof <- hIsEOF h
              if eof 
                  then  eject
                  else do  
                        !chunk  <- hGetArray h len
                        eat chunk
        {-# INLINE pull_sourceBytes #-}
{-# INLINE [2] sourceBytes #-}


-- Sinking ----------------------------------------------------------------------------------------
-- | Open from files for writing and use the handles to create `Sinks`.
--
--   Finalisers are attached to the sinks so that file assocated with
--   each stream is closed when that stream is ejected.
--
toFiles :: [FilePath] 
        -> ([Handle] -> IO (Sinks Int IO a))
        -> IO (Sinks Int IO a)

toFiles paths use
 = do   hs      <- mapM (flip openBinaryFile WriteMode) paths
        o0      <- use hs
        finalize_o (\(IIx i _) -> hClose (hs !! i)) o0
{-# NOINLINE toFiles #-}


-- | Write chunks of 8-byte ASCII characters to the given file handles.
sinkChars :: [Handle] -> IO (Sinks Int IO (Vector F Char))
sinkChars !hs
 =   smap_o (\_ !c -> computeS_ $ A.map (fromIntegral . ord) c)
 =<< sinkBytes hs
{-# INLINE [2] sinkChars #-}


-- | Write chunks of bytes to the given file handles.
sinkBytes :: [Handle] -> IO (Sinks Int IO (Vector F Word8))
sinkBytes !hs
 = do   let push_sinkBytes (IIx i _) !chunk
                = hPutArray (hs !! i) chunk
            {-# NOINLINE push_sinkBytes #-}

        let eject_sinkBytes _
                = return ()
            {-# INLINE eject_sinkBytes #-}

        return  $ Sinks (P.length hs) push_sinkBytes eject_sinkBytes
{-# INLINE [2] sinkBytes #-}

