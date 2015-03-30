
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
        )
where
import Data.Repa.Flow.Auto
import Data.Repa.Flow.IO.Bucket
import Data.Repa.Array.Generic                  as A
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
        =   G.map_i (A.convert A)
        =<< G.sourceBytes i bs
{-# INLINE sourceBytes #-}


-- | Like `F.sourceChars`, but with the default chunk size.
sourceChars 
        :: Integer -> Array B Bucket -> IO (Sources Char)
sourceChars i bs 
        =   G.map_i (A.convert A)
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
 =   G.map_i ((A.convert A). chopChunk)
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
        =   G.map_i (A.convert A)
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
        =   G.map_i (A.convert A)
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
        =   G.map_i (A.convert A)
        =<< G.sourceTSV i aFail bs
{-# INLINE sourceTSV #-}

