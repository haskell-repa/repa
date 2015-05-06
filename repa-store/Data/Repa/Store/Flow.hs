
module Data.Repa.Store.Flow
        ( sourceTableFormat
        , sourceFamilyColumn

          -- * Delimitors
        , Delim (..)

        , module Data.Repa.Convert.Formats)
where
import Data.Repa.Store.Format                   as Format
import Data.Repa.Store.Partitions

import Data.Repa.Convert.Formats
import Data.Repa.Flow.Generic.IO                as G
import Data.Repa.Flow.Generic                   as F
import Data.Repa.Array.Generic                  as A
import Data.Repa.Array.Material                 as A
import Data.Repa.Convert
import Data.Word
#include "repa-store.h"


---------------------------------------------------------------------------------------------------
-- | Source complete rows from a table.
sourceTableFormat
        :: forall format
        .  ( Packable (Sep format), Target A (Value format)
           , Value (Sep format) ~ Value format
           , Show format)
        => Integer                      -- ^ Chunk length.
        -> IO ()                        -- ^ Action if we find an over-long line.
        -> IO (Array A Word8 -> IO ())  -- ^ Action if we can't convert a row.
        -> FilePath                     -- ^ Path to table directory.
        -> Delim                        -- ^ Row delimitor.
        -> format                       -- ^ Row format.
        -> IO (Sources Int IO (Array A (Value format)))

sourceTableFormat
        nChunk aFailLong aFailConvert 
        path delim format
 | LinesSep c   <- delim
 = do  
        -- TODO: check directory exists.
        Just parts  <- listPartitions path
        ss          <- fromFiles parts 
                    $  sourceLinesFormat nChunk aFailLong aFailConvert 
                                (Sep c format)

        return ss

 | otherwise
 = error "sourceTable: TODO finish me"
{-# INLINE_FLOW sourceTableFormat #-}


---------------------------------------------------------------------------------------------------
-- | Source elements from a single column in a column family. 
sourceFamilyColumn 
        :: ( Packable format
           , Target A (Value format)
           , Show format)
        => Integer
        -> IO ()                        -- ^ Action if we find an over-long line.
        -> IO (Array A Word8 -> IO ())  -- ^ Action if we can't convert a row.
        -> FilePath                     -- ^ Path to column directory.
        -> format                       -- ^ Element format.
        -> IO (Sources Int IO (Array A (Value format)))

sourceFamilyColumn 
        nChunk aFailLong aFailConvert
        path format
 = do   
        -- TODO: check directory exists
        Just parts <- listPartitions path

        ss         <- fromFiles parts
                   $  sourceLinesFormat nChunk aFailLong aFailConvert
                                format

        return ss

