
module Data.Repa.Store.Flow
        ( sourceTableFormat

        , sourceFamilyKey
        , sourceFamilyColumn

          -- * Delimitors
        , Format.Delim (..)

        , module Data.Repa.Convert.Formats)
where
import System.FilePath
import Data.Repa.Fusion.Unpack
import Data.Repa.Store.Partitions
import Data.Repa.Array.Generic.Target
import Data.Repa.Convert.Formats
import Data.Repa.Flow.Generic.IO                as G
import Data.Repa.Flow.Generic                   as FG
import Data.Repa.Array.Generic                  as A
import Data.Repa.Array.Material                 as A
import Data.Repa.Array.Meta.Window
import qualified Data.Repa.Flow.Auto            as F
import qualified Data.Repa.Store.Format         as Format
import Data.Repa.Convert
import Data.Word
#include "repa-store.h"


---------------------------------------------------------------------------------------------------
-- | Source complete rows from a table.
sourceTableFormat
        :: forall format
        .  ( Packable (Sep format), Target A (Value format)
           , SepFormat format
           , Value (Sep format) ~ Value format)
        => Integer                      -- ^ Chunk length.
        -> IO ()                        -- ^ Action if we find an over-long line.
        -> IO (Array A Word8 -> IO ())  -- ^ Action if we can't convert a row.
        -> FilePath                     -- ^ Path to table directory.
        -> Format.Delim                 -- ^ Row delimitor.
        -> format                       -- ^ Row format.
        -> IO (Sources Int IO (Array A (Value format)))

sourceTableFormat
        nChunk aFailLong aFailConvert 
        path delim format
 | Format.LinesSep c   <- delim
 = do  
        -- TODO: check directory exists.
        Just parts  <- listPartitions path
        ss          <- fromFiles parts 
                    $  sourceLinesFormat 
                                nChunk aFailLong aFailConvert 
                                (mkSep c format)

        return ss

 | otherwise
 = error "sourceTable: TODO finish me"
{-# INLINE_FLOW sourceTableFormat #-}


---------------------------------------------------------------------------------------------------
-- | Source the shape of a column family.
--
--   This uses replicates the shape information, yielding
--   a flow of the same length as the associated columns.
sourceFamilyKey
        :: ( Packable format
           , Bulk   A (Value format)
           , Target A (Value format)
           , Packable (Sep (format :*: IntAsc :*: ()))
           , Show format
           , Windowable A (Value format)
           , Unpack (Buffer A (Value format)) tf)
        => Integer
        -> IO ()                        -- ^ Action if we find an over-long line.
        -> IO (Array A Word8 -> IO ())  -- ^ Action if we can't convet a row.
        -> FilePath                     -- ^ Path to family directory.
        -> format                       -- ^ Format of family key.
        -> IO (Sources Int IO (Array A (Value format)))

sourceFamilyKey
        nChunk aFailLong aFailConvert
        path format
 = do   
        -- TODO: check directory exists.
        let dirShape    = path </> "_shape"
        Just parts      <- listPartitions dirShape

        ssShape <- fromFiles parts
                $  sourceLinesFormat nChunk aFailLong aFailConvert 
                        (mkSep '\t' (format :*: IntAsc :*: ()))

        ssKey   <-  F.replicates_i 
                =<< F.map_i (\(v :*: c :*: ()) -> (c, v)) ssShape       -- TODO: chunkwise.

        return ssKey


-- | Source elements from a single column in a column family.
--
--   This produces the column 
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



