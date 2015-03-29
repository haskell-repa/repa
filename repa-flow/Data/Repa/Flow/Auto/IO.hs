
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
        , sourcePacked

          -- * Sinking
        , sinkBytes
        , sinkLines
        , sinkChars
        , sinkPacked
        )
where
import Data.Repa.Flow.Auto
import Data.Repa.Flow.IO.Bucket
import Data.Repa.Array.Material                 as A
import Data.Repa.Array.Auto.Unpack              as A
import Data.Repa.Array.Generic.Convert          as A
import Data.Repa.Array.Generic                  as A
import Data.Word
import qualified Data.Repa.Flow.Generic         as G
import qualified Data.Repa.Flow.Generic.IO      as G
import qualified Data.Repa.Flow.Auto.SizedIO    as F
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


-- | Read packed binary data from some buckets and unpack the values
--   to some `Sources`.
--
-- The following uses the @colors.bin@ file produced by the `sinkPacked` example:
--
-- @
-- > import Data.Repa.Flow            as F
-- > import Data.Repa.Convert.Format  as F
--
-- > let format = FixString ASCII 10 :*: Float64be :*: Int16be
-- > ss <- fromFiles' [\"colors.bin\"] $ sourcePacked format (error \"convert failed\")
--
-- > toList1 0 ss
-- [\"red\" :*: (5.3 :*: 100), \"green\" :*: (2.8 :*: 93), \"blue\" :*: (0.99 :*: 42)]
-- @
--
sourcePacked
        :: (Packable format, Target A (Value format))
        => format                       -- ^ Binary format for each value.
        -> IO ()                        -- ^ Action when a value cannot be converted.
        -> Array B Bucket               -- ^ Input buckets.
        -> IO (Sources (Value format))

sourcePacked format aFail bs
 = return $ G.Sources (A.length bs) pull_sourcePacked
 where
        pull_sourcePacked i eat eject
         = do let b = A.index bs i
              op <- bIsOpen b
              if not op 
               then eject
               else do
                eof <- bAtEnd b
                if eof
                 then eject
                 else do
                        !chunk  <- bGetArray b defaultChunkSize
                        case A.unpackForeign format (A.convert chunk) of
                         Nothing        -> aFail
                         Just vals      -> eat vals
        {-# INLINE pull_sourcePacked #-}
{-# INLINE_FLOW sourcePacked #-}


---------------------------------------------------------------------------------------------------
-- | Write 8-bit bytes to some files.
sinkBytes :: Array B Bucket -> IO (Sinks Word8)
sinkBytes bs
        =   G.map_o A.convert
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


-- | Convert values to a packed binary format and write them
--   them to some buckets.
--
-- @
-- > import Data.Repa.Flow           as F
-- > import Data.Repa.Convert.Format as F
--
-- > let format = FixString ASCII 10 :*: Float64be :*: Int16be
-- > let vals   = listFormat format
--                 [ \"red\"   :*: 5.3    :*: 100
--                 , \"green\" :*: 2.8    :*: 93 
--                 , \"blue\"  :*: 0.99   :*: 42 ]
--
-- > ss  <- F.fromList 1 vals
-- > out <- toFiles' [\"colors.bin\"] $ sinkPacked format (error \"convert failed\")
-- > drainS ss out
-- @
--
sinkPacked 
        :: (Packable format, Bulk A (Value format))
        => format                       -- ^ Binary format for each value.
        -> IO ()                        -- ^ Action when a value cannot be converted.
        -> Array B Bucket               -- ^ Output buckets.
        -> IO (Sinks (Value format))

sinkPacked format aFail bs
 = return $ G.Sinks (A.length bs) push_sinkPacked eject_sinkPacked
 where  
        push_sinkPacked i !chunk
         = case A.packForeign format chunk of
                Nothing   -> aFail
                Just buf  -> bPutArray (bs `index` i) (A.convert buf)
        {-# INLINE push_sinkPacked #-}

        eject_sinkPacked i 
         = bClose (bs `index` i)
        {-# INLINE eject_sinkPacked #-}

{-# INLINE_FLOW sinkPacked #-}

