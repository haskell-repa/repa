
-- | Re-exports functions from "Data.Repa.Flow.IO" that use a default
--   chunk size of 64kBytes and just call `error` if the source file
--   appears corruped. 
module Data.Repa.Flow.IO.Default
        ( defaultChunkSize

          -- * Sourcing
        , F.fromFiles
        , F.bucketsFromFile
        , sourceTSV
        , sourceRecords
        , sourceLines
        , sourceChars
        , sourceBytes

          -- * Sinking
        , F.toFiles
        , sinkChars
        , sinkLines
        , sinkBytes)
where
import qualified Data.Repa.Flow.IO              as F
import qualified Data.Repa.Flow.IO.Bucket       as F
#include "repa-stream.h"


-- | The default chunk size of 64kBytes.
defaultChunkSize :: Integer
defaultChunkSize = 64 * 1024

-- | Like `F.sourceTSV`, but with the default chunk size and error action.
sourceTSV
        = F.sourceTSV defaultChunkSize
        $ error $  "Line exceeds chunk size of "
                ++ show defaultChunkSize ++ "bytes."

-- | Like `F.sourceRecords`, but with the default chunk size and error action.
sourceRecords pSep 
        = F.sourceRecords defaultChunkSize pSep
        $ error $  "Record exceeds chunk size of " 
                ++ show defaultChunkSize ++ "bytes."

-- | Like `F.sourceLines`, but with the default chunk size and error action.
sourceLines     
        = F.sourceLines   defaultChunkSize
        $ error $  "Line exceeds chunk size of "
                ++ show defaultChunkSize ++ "bytes."

-- | Like `F.sourceChars`, but with the default chunk size.
sourceChars     = F.sourceChars defaultChunkSize

-- | Like `F.sourceBytes`, but with the default chunk size.
sourceBytes     = F.sourceBytes defaultChunkSize

-- | An alias for `F.sinkLines`.
sinkLines       = F.sinkLines

-- | An alias for `F.sinkChars`.
sinkChars       = F.sinkChars

-- | An alias for `F.sinkBytes`.
sinkBytes       = F.sinkBytes

