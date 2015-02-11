
-- | Read and write files.
module Data.Repa.Flow.Default.SizedIO
        ( module Data.Repa.Flow.IO.Bucket

           -- * Sourcing
        , fromFiles
        , sourceBytes
        , sourceChars
        , sourceLines
        , sourceRecords
        , sourceTSV

          -- * Sinking
        , toFiles
        , sinkBytes
        , sinkChars
        , sinkLines)
where
import Data.Repa.Flow.Default
import Data.Repa.Flow.IO.Bucket
import Data.Repa.Flow.Default.IO.TSV            as F
import Data.Repa.Eval.Array                     as A
import Data.Repa.Array.Material                 as A
import Data.Repa.Fusion.Unpack                  as F
import Data.Repa.Array                          as A 
import qualified Data.Repa.Flow.Generic.IO      as G
import Data.Word
import Data.Char
#include "repa-flow.h"


-- Sourcing ---------------------------------------------------------------------------------------
-- | Open some files as buckets and use them as `Sources`.
--
--   Finalisers are attached to the `Sources` so that each file will be 
--   closed the first time the consumer tries to an element from the associated
--   stream when no more are available.
--
fromFiles
        :: [FilePath]           -- ^ Files to open.
        -> ([Bucket] -> IO (Sources l a))
        -> IO (Sources l a)
fromFiles paths use
 = G.fromFiles (A.fromList B paths) use'
 where  use' arr = use (A.toList arr)
{-# INLINE fromFiles #-}


-- | Like `F.sourceBytes`, but with the default chunk size.
sourceBytes 
        :: Integer -> [Bucket] -> IO (Sources F Word8)
sourceBytes i bs = G.sourceBytes i (A.fromList B bs)
{-# INLINE sourceBytes #-}


-- | Like `F.sourceChars`, but with the default chunk size.
sourceChars 
        :: Integer -> [Bucket] -> IO (Sources F Char)
sourceChars i bs = G.sourceChars i (A.fromList B bs)
{-# INLINE sourceChars #-}


-- | Like `F.sourceLines`, but with the default chunk size and error action.
sourceLines
        ::Integer               -- ^ Size of chunk to read in bytes.
        -> IO ()                -- ^ Action to perform if we can't get a
                                --   whole record.
        -> [Bucket]             -- ^ Buckets.
        -> IO (Sources N (Array F Char))
sourceLines nChunk fails bs
 =   mapChunks_i chopChunk
 =<< G.sourceRecords nChunk isNewLine fails (A.fromList B bs)
 where
        isNewLine   :: Word8 -> Bool
        isNewLine x =  x == nl
        {-# INLINE isNewLine #-}
  
        chopChunk chunk
         = A.mapElems (A.computeS name . A.map (chr . fromIntegral)) 
         $ A.trimEnds (== nl) chunk
        {-# INLINE chopChunk #-}

        nl :: Word8
        !nl = fromIntegral $ ord '\n'
{-# NOINLINE sourceLines #-}


-- | Like `F.sourceRecords`, but with the default chunk size and error action.
sourceRecords 
        :: Integer              -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a
                                --   whole record.
        -> [Bucket]             -- ^ File handles.
        -> IO (Sources N (Array F Word8))
sourceRecords i pSep aFail bs 
        = G.sourceRecords i pSep aFail (A.fromList B bs)
{-# INLINE sourceRecords #-}


-- Sinking ----------------------------------------------------------------------------------------
-- | Open from files for writing and use the handles to create `Sinks`.
--
--   Finalisers are attached to the sinks so that file assocated with
--   each stream is closed when that stream is ejected.
toFiles :: [FilePath]
        -> ([Bucket] -> IO (Sinks l a))
        -> IO (Sinks l a)
toFiles paths use
 = G.toFiles (A.fromList B paths) use'
 where  use' arr = use (A.toList arr)
{-# INLINE toFiles #-}


-- | An alias for `F.sinkBytes`.
sinkBytes 
        :: [Bucket] -> IO (Sinks F Word8)
sinkBytes bs = G.sinkBytes (A.fromList B bs)
{-# INLINE sinkBytes #-}


-- | An alias for `F.sinkChars`.
sinkChars 
        :: [Bucket] -> IO (Sinks F Char)
sinkChars bs 
        = G.sinkChars (A.fromList B bs)
{-# INLINE sinkChars #-}


-- | An alias for `F.sinkLines`.
sinkLines 
        :: ( BulkI l1 (Array l2 Char)
           , BulkI l2 Char, Unpack (Array l2 Char) t2)
        => Name  l1                     -- ^ Layout for chunks of lines.
        -> Name  l2                     -- ^ Layout for lines.
        -> [Bucket]                     -- ^ Buckets
        -> IO (Sinks l1 (Array l2 Char))
sinkLines n1 n2 bs 
        = G.sinkLines n1 n2 (A.fromList B bs)
{-# INLINE sinkLines #-}

