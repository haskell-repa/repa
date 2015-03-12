
-- | Read and write files.
module Data.Repa.Flow.Default.SizedIO
        ( module Data.Repa.Flow.IO.Bucket

           -- * Sourcing
        , sourceBytes
        , sourceChars
        , sourceLines
        , sourceRecords
        , G.sourceTSV
        , G.sourceCSV

          -- * Sinking
        , sinkBytes
        , sinkChars
        , sinkLines)
where
import Data.Repa.Flow.Default
import Data.Repa.Flow.IO.Bucket
import Data.Repa.Eval.Array                     as A
import Data.Repa.Array.Material                 as A
import Data.Repa.Fusion.Unpack                  as F
import Data.Repa.Array                          as A 
import qualified Data.Repa.Flow.Generic         as G
import qualified Data.Repa.Flow.Generic.IO      as G
import Data.Word
import Data.Char
#include "repa-flow.h"


-- Sourcing ---------------------------------------------------------------------------------------
-- | Like `F.sourceBytes`, but with the default chunk size.
sourceBytes 
        :: BulkI l Bucket
        => Integer -> Array l Bucket -> IO (Sources F Word8)
sourceBytes i bs = G.sourceBytes i bs
{-# INLINE sourceBytes #-}


-- | Like `F.sourceChars`, but with the default chunk size.
sourceChars 
        :: BulkI l Bucket
        => Integer -> Array l Bucket -> IO (Sources F Char)
sourceChars i bs = G.sourceChars i bs
{-# INLINE sourceChars #-}


-- | Like `F.sourceLines`, but with the default chunk size and error action.
sourceLines
        :: BulkI l Bucket
        => Integer               -- ^ Size of chunk to read in bytes.
        -> IO ()                -- ^ Action to perform if we can't get a
                                --   whole record.
        -> Array l Bucket       -- ^ Buckets.
        -> IO (Sources N (Array F Char))
sourceLines nChunk fails bs
 =   G.map_i chopChunk
 =<< G.sourceRecords nChunk isNewLine fails bs
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
        :: BulkI l Bucket
        => Integer              -- ^ Size of chunk to read in bytes.
        -> (Word8 -> Bool)      -- ^ Detect the end of a record.        
        -> IO ()                -- ^ Action to perform if we can't get a
                                --   whole record.
        -> Array l Bucket       -- ^ File handles.
        -> IO (Sources N (Array F Word8))
sourceRecords i pSep aFail bs 
        = G.sourceRecords i pSep aFail bs
{-# INLINE sourceRecords #-}


-- Sinking ----------------------------------------------------------------------------------------
-- | An alias for `F.sinkBytes`.
sinkBytes 
        :: BulkI l Bucket
        => Array l Bucket -> IO (Sinks F Word8)
sinkBytes bs = G.sinkBytes bs
{-# INLINE sinkBytes #-}


-- | An alias for `F.sinkChars`.
sinkChars 
        :: BulkI l Bucket
        => Array l Bucket -> IO (Sinks F Char)
sinkChars bs = G.sinkChars bs
{-# INLINE sinkChars #-}


-- | An alias for `F.sinkLines`.
sinkLines 
        :: ( BulkI l  Bucket
           , BulkI l1 (Array l2 Char)
           , BulkI l2 Char, Unpack (Array l2 Char) t2)
        => Name  l1                     -- ^ Layout for chunks of lines.
        -> Name  l2                     -- ^ Layout for lines.
        -> Array l Bucket               -- ^ Buckets
        -> IO (Sinks l1 (Array l2 Char))
sinkLines n1 n2 bs 
        = G.sinkLines n1 n2 bs
{-# INLINE sinkLines #-}

