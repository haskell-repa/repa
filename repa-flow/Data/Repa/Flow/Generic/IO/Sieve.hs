
module Data.Repa.Flow.Generic.IO.Sieve
        (sieve_o)
where
import Data.Repa.Flow.Generic.Base
import Data.Repa.Array.Material         as A
import Data.Repa.Array.Generic          as A
import Data.Repa.Array.Auto.IO          as A
import Data.Sequence                    (Seq)
import qualified Data.Sequence          as Seq
import qualified Data.Foldable          as Fold
import qualified Data.HashTable.IO      as Hash
import qualified System.Mem             as System
import System.IO
import Data.Word
import Data.IORef
#include "repa-flow.h"


-- | Create an output sieve that writes data to an indeterminate number of
--   output files. Each new element is appended to its associated file.
--
---
--   We don't want to open and close a file every time we receieve data.
--   To avoid this, we instead batch data in memory for each file until
--   we have enough to warrant performing the IO operation.
--
sieve_o :: Int                  -- ^ Max amount of in-memory data.
        -> (a -> Maybe (FilePath, Array F Word8))   
                                -- ^ Produce the desired file path and output
                                --   record for this element, or `Nothing` if
                                --   it should be discarded.
        -> IO (Sinks () IO a)

sieve_o sizeLimit diag
 = do
        (ht :: Hash.CuckooHashTable FilePath (Seq (Array F Word8)))
         <- Hash.newSized 1024

        refSize <- newIORef 0

        let flush_path (path, ss)
             = do h     <- openBinaryFile path AppendMode 
                  mapM_ (hPutArray h . convert A) $ Fold.toList ss
                  Hash.delete ht path
                  hClose h

        let flush_all 
             = do Hash.mapM_ flush_path ht
                  System.performMajorGC

        let acc_size len
             = do sizeCurrent  <- readIORef refSize
                  if sizeCurrent + len > sizeLimit
                   then do
                        flush_all
                        writeIORef refSize 0

                   else do
                        writeIORef refSize (sizeCurrent + len)

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
                                acc_size (A.length arr)

                         -- We already have data for this file.
                         Just ss
                          -> do Hash.insert ht path (ss Seq.|> arr)
                                acc_size (A.length arr)

        let eject_sieve _ 
             = flush_all

        return  $ Sinks () push_sieve eject_sieve
{-# INLINE sieve_o #-}

