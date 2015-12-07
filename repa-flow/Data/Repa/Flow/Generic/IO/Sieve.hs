
module Data.Repa.Flow.Generic.IO.Sieve
        (sieve_o)
where
import Data.Repa.Flow.Generic.Base
import Data.Repa.Array.Material         as A
import Data.Repa.Array.Generic          as A
import Data.Repa.Array.Auto.IO          as A
import qualified Data.HashTable.IO      as Hash
import qualified System.Mem             as System
import System.IO
import Data.Word
import Data.IORef

import qualified Data.Vector.Mutable    as M
import qualified Data.Vector            as V

#include "repa-flow.h"


-- | Create an output sieve that writes data to an indeterminate number of
--   output files. Each new element is appended to its associated file.
--
---
--   We don't want to open and close a file every time we receieve data.
--   To avoid this, we instead batch data in memory for each file until
--   we have enough to warrant performing the IO operation.
--
sieve_o :: Int                  -- ^ Max payload size of in-memory data.
        -> Int                  -- ^ Max number of in-memory chunks.
        -> (a -> Maybe (FilePath, Array F Word8))   
                                -- ^ Produce the desired file path and output
                                --   record for this element, or `Nothing` if
                                --   it should be discarded.
        -> IO (Sinks () IO a)

sieve_o sizeLimit chunksLimit diag
 = do
        (ht :: Hash.CuckooHashTable FilePath (Int, M.IOVector (Array F Word8)))
         <- Hash.newSized 1024

        !refSize   <- newIORef 0
        !refChunks <- newIORef 0

        let flush_path (path, (n, mvec))
             = do 
                  !vec   <- V.unsafeFreeze mvec
                  !h     <- openBinaryFile path AppendMode 

                  V.mapM_ (hPutArray h . convert A) 
                        $ V.slice 0 n vec

                  Hash.delete ht path
                  hClose h

        let flush_all 
             = do Hash.mapM_ flush_path ht

        let acc_size !len
             = do !sizeCurrent    <- readIORef refSize
                  !chunksCurrent  <- readIORef refChunks
                  if  (sizeCurrent   + len) > sizeLimit
                   || (chunksCurrent + 1)   > chunksLimit
                   then do
                        flush_all
                        writeIORef refSize   0
                        writeIORef refChunks 0

                   else do
                        let !sizeCurrent'   = sizeCurrent + len
                        let !chunksCurrent' = chunksCurrent + 1
                        writeIORef refSize   sizeCurrent'
                        writeIORef refChunks chunksCurrent'

        let push_sieve _ !e
             = case diag e of
                -- Drop this element on the floor.
                Nothing 
                 -> return ()

                -- Accumulate a new chunk.
                Just (path, arr)
                 -> do  !mElem   <- Hash.lookup ht path
                        case mElem of

                         -- We haven't seen this file before, 
                         -- so create a new vector to hold the chunks.
                         Nothing
                          -> do !mvec    <- M.new 256
                                M.write mvec 0 arr
                                Hash.insert ht path (1, mvec)
                                acc_size (A.length arr)

                         -- We already have data for this file.
                         Just (n, mvec)
                          -> do !mvec'  <- if n >= M.length mvec
                                                then M.grow mvec (M.length mvec)
                                                else return mvec

                                M.write mvec' n arr
                                let !n' = n + 1
                                Hash.insert ht path (n', mvec')
                                acc_size (A.length arr)

        let eject_sieve _ 
             = do flush_all
                  System.performMajorGC


        return  $ Sinks () push_sieve eject_sieve
{-# INLINE sieve_o #-}

