
module Data.Repa.Flow.Generic.IO.Lines
        (sourceLinesFormat)
where
import Data.Repa.Flow.Generic.IO.Base           as F
import Data.Repa.Flow.Generic.Map               as F
import Data.Repa.Flow.Generic.Base              as F
import Data.Repa.Array.Generic                  as A
import Data.Repa.Array.Material                 as A
import qualified Data.Repa.Array.Auto.Format    as A
import Data.Repa.Convert.Format                 as C
import Data.Char
import Data.Word
#include "repa-flow.h"


-- | Read a the lines of a text file,
--   converting each line to values with the given format.
sourceLinesFormat
        :: forall format
        .  (Packable format, Target A (Value format))
        => Integer                      -- ^ Chunk length.
        -> IO ()                        -- ^ Action if we find a line longer than the chunk length.
        -> IO (Array A Word8 -> IO ())  -- ^ Action if we can't convert a row.
        -> format                       -- ^ Format of each line.
        -> Array B Bucket
        -> IO (Sources Int IO (Array A (Value format)))

sourceLinesFormat nChunk aFailLong _aFailConvert format bs
 = do
        -- Rows are separated by new lines.
        let !nl  = fromIntegral $ ord '\n'
        let !nr  = fromIntegral $ ord '\r'

        -- Stream chunks of data from the input file, 
        -- where the chunks end cleanly and line boundaries.
        -- Filter out any stray CR characters allong the way.
        sChunk  <- sourceChunks nChunk (== nl) aFailLong bs

        sRows8  :: Sources Int IO (Array N (Array F Word8))
                <- map_i ( A.trimEnds  (== nl)
                         . A.segmentOn (== nl) 
                         . A.filter F  (/= nr)) 
                         sChunk

        -- Convert each value using the given format.
        let unpackRow :: Array A Word8 -> Value format
            unpackRow arr
             = case A.unpackFormat format arr of
                 Nothing -> error ("no convert " ++ show arr)
                        -- TODO: impl proper pull function
                        -- so we can call aFailConvert if needed.
                        -- We shouldn't be throwing errors this deep in the library.
                 Just v  -> v
            {-# INLINE unpackRow #-}

        F.map_i (A.mapS A (unpackRow . A.convert A)) sRows8
{-# INLINE sourceLinesFormat #-}

