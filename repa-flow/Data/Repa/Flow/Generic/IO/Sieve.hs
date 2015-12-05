
module Data.Repa.Flow.Generic.IO.Sieve
        (sieve_o)
where
import Data.Repa.Flow.Generic.Base
import Data.Repa.Array.Material         as A
import Data.Repa.Array.Generic.Convert  as A
import Data.Repa.Array.Auto.IO          as A
import Data.Sequence                    (Seq)
import qualified Data.Sequence          as Seq
import qualified Data.Foldable          as Fold
import qualified Data.HashTable.IO      as Hash
import System.IO
import Data.Word
#include "repa-flow.h"


-- | Create an output sieve that writes data to an indeterminate number of
--   output files. Each new element is appended to its associated file.
--
---
--   We don't want to open and close a file every time we receieve data.
--   To avoid this, we instead batch data in memory for each file until
--   we have enough to warrant performing the IO operation.
--
sieve_o :: (a -> Maybe (FilePath, Array F Word8))   
                                -- ^ Produce the desired file path and output
                                --   record for this element, or `Nothing` if
                                --   it should be discarded.
        -> IO (Sinks () IO a)

sieve_o diag
 = do
        (ht :: Hash.CuckooHashTable FilePath (Seq (Array F Word8)))
         <- Hash.newSized 1024

        let flush_path (path, ss)
             = do h     <- openBinaryFile path AppendMode 
                  mapM_ (hPutArray h . convert) $ Fold.toList ss
                  Hash.delete ht path
                  hClose h

        let push_sieve _ e
             = case diag e of
                -- Drop this element on the floor.
                Nothing 
                 -> return ()

                -- Accumulate a new chunk.
                Just (path, arr)
                 -> do  mElem   <- Hash.lookup ht path
                        case mElem of
                         -- We haven't seen this file before, 
                         -- so insert the element into the table.
                         Nothing
                          -> do Hash.insert ht path (Seq.singleton arr)
                                return ()

                         -- We already have data for this file.
                         -- If we already have more than 100 chunks then flush them all,
                         -- otherwise accumulate into the in-memory state.
                         --
                         -- TODO: batch by write size instead of number of chunks.
                         -- TODO: also flush if the total size of the hash table 
                         --       becomes too large.
                         Just ss
                          | Seq.length ss > 100
                          ->    flush_path (path, ss)

                          | otherwise
                          ->    Hash.insert ht path (ss Seq.|> arr)

        let eject_sieve _ 
             = Hash.mapM_ flush_path ht

        return  $ Sinks () push_sieve eject_sieve
{-# INLINE sieve_o #-}

