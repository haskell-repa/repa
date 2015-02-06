
-- | Read and write files.
module Data.Repa.Flow.Default.SizedIO
        ( module Data.Repa.Flow.IO.Bucket

           -- * Sourcing
        , G.fromFiles
        , sourceBytes
        , sourceChars
        , sourceLines
        , sourceRecords
        , sourceTSV

          -- * Sinking
        , G.toFiles
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
import Data.Repa.Array                          as A hiding (fromList)
import qualified Data.Repa.Flow.Generic.IO      as G
import Data.Word
import Data.Char
#include "repa-stream.h"


-- Sourcing ---------------------------------------------------------------------------------------
-- | Like `F.sourceBytes`, but with the default chunk size.
sourceBytes :: Integer -> [Bucket] -> IO (Sources F Word8)
sourceBytes = G.sourceBytes
{-# INLINE sourceBytes #-}


-- | Like `F.sourceChars`, but with the default chunk size.
sourceChars :: Integer -> [Bucket] -> IO (Sources F Char)
sourceChars = G.sourceChars
{-# INLINE sourceChars #-}


-- | Like `F.sourceLines`, but with the default chunk size and error action.
sourceLines
        :: Integer              -- ^ Size of chunk to read in bytes.
        -> IO ()                -- ^ Action to perform if we can't get a
                                --   whole record.
        -> [Bucket]             -- ^ File handles.
        -> IO (Sources N (Array F Char))
sourceLines nChunk fails hs
 =   mapChunks_i chopChunk
 =<< G.sourceRecords nChunk isNewLine fails hs
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
{-# INLINE sourceLines #-}


-- | Like `F.sourceRecords`, but with the default chunk size and error action.
sourceRecords 
        :: Integer              -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a
                                --   whole record.
        -> [Bucket]             -- ^ File handles.
        -> IO (Sources N (Array F Word8))
sourceRecords = G.sourceRecords
{-# INLINE sourceRecords #-}


-- Sinking ----------------------------------------------------------------------------------------
-- | An alias for `F.sinkBytes`.
sinkBytes :: [Bucket] -> IO (Sinks F Word8)
sinkBytes = G.sinkBytes
{-# INLINE sinkBytes #-}


-- | An alias for `F.sinkChars`.
sinkChars :: [Bucket] -> IO (Sinks F Char)
sinkChars = G.sinkChars
{-# INLINE sinkChars #-}


-- | An alias for `F.sinkLines`.
sinkLines :: ( BulkI l1 (Array l2 Char)
             , BulkI l2 Char, Unpack (Array l2 Char) t2)
          => Name l1 -> Name l2
          -> [Bucket]           -- ^ Buckets
          -> IO (Sinks l1 (Array l2 Char))
sinkLines = G.sinkLines
{-# INLINE sinkLines #-}

