
-- | Read and write files.
module Data.Repa.Flow.Auto.SizedIO
        ( -- * Buckets
          module Data.Repa.Flow.IO.Bucket

          -- * Sourcing
        , sourceBytes
        , sourceChars
        , sourceLines
        , sourceRecords
        , sourceTSV
        , sourceCSV

          -- * Sinking
        , sinkBytes
        , sinkChars
        , sinkLines
        )
where
import Data.Repa.Flow.Auto
import Data.Repa.Flow.IO.Bucket
import Data.Repa.Array.Generic                  as A
import Data.Repa.Array.Generic.Convert          as A
import Data.Repa.Array.Material                 as A
import Data.Repa.Array.Meta.Delayed             as A
import qualified Data.Repa.Flow.Generic         as G
import qualified Data.Repa.Flow.Generic.IO      as G
import Data.Word
import Data.Char
#include "repa-flow.h"


-- Sourcing ---------------------------------------------------------------------------------------
-- | Like `F.sourceBytes`, but with the default chunk size.
sourceBytes 
        :: Integer -> Array B Bucket -> IO (Sources Word8)
sourceBytes i bs 
        =   G.map_i A.convert
        =<< G.sourceBytes i bs
{-# INLINE sourceBytes #-}


-- | Like `F.sourceChars`, but with the default chunk size.
sourceChars 
        :: Integer -> Array B Bucket -> IO (Sources Char)
sourceChars i bs 
        =   G.map_i A.convert
        =<< G.sourceChars i bs
{-# INLINE sourceChars #-}


-- | Like `F.sourceLines`, but with the default chunk size and error action.
sourceLines
        :: Integer              -- ^ Size of chunk to read in bytes.
        -> IO ()                -- ^ Action to perform if we can't get a
                                --   whole record.
        -> Array B Bucket       -- ^ Buckets.
        -> IO (Sources (Array A Char))

sourceLines nChunk fails bs
 =   G.map_i (A.convert . chopChunk)
 =<< G.sourceRecords nChunk isNewLine fails bs
 where
        isNewLine   :: Word8 -> Bool
        isNewLine x =  x == nl
        {-# INLINE isNewLine #-}
  
        chopChunk chunk
         = A.mapElems (A.computeS A . A.map (chr . fromIntegral)) 
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
        -> Array B Bucket       -- ^ File handles.
        -> IO (Sources (Array A Word8))

sourceRecords i pSep aFail bs 
        =   G.map_i A.convert
        =<< G.sourceRecords i pSep aFail bs
{-# INLINE sourceRecords #-}


-- | Read a file containing Comma-Separated-Values.
sourceCSV
        :: Integer              -- ^ Chunk length.
        -> IO ()                -- ^ Action to perform if we find line longer
                                --   than the chunk length.
        -> Array B Bucket       -- ^ Buckets
        -> IO (Sources (Array A (Array A Char)))

sourceCSV i aFail bs
        =   G.map_i A.convert
        =<< G.sourceCSV i aFail bs
{-# INLINE sourceCSV #-}


-- | Read a file containing Tab-Separated-Values.
sourceTSV
        :: Integer              -- ^ Chunk length.
        -> IO ()                -- ^ Action to perform if we find line longer
                                --   than the chunk length.
        -> Array B Bucket       -- ^ Buckets
        -> IO (Sources (Array A (Array A Char)))

sourceTSV i aFail bs
        =   G.map_i A.convert
        =<< G.sourceTSV i aFail bs
{-# INLINE sourceTSV #-}


-- Sinking ----------------------------------------------------------------------------------------
-- | An alias for `F.sinkBytes`.
sinkBytes :: Array B Bucket -> IO (Sinks Word8)
sinkBytes bs 
        =   G.map_o A.convert
        =<< G.sinkBytes bs
{-# INLINE sinkBytes #-}


-- | An alias for `F.sinkChars`.
sinkChars  :: Array B Bucket -> IO (Sinks Char)
sinkChars bs 
        = G.sinkChars bs
{-# INLINE sinkChars #-}


-- | An alias for `F.sinkLines`.
sinkLines  :: Array B Bucket
           -> IO (Sinks (Array A Char))
sinkLines bs 
        = G.sinkLines A A bs
{-# INLINE sinkLines #-}
