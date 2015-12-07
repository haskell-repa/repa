
module Data.Repa.Flow.Generic.IO.Sieve
        (sieve_o)
where
import Data.Repa.Flow.Generic.Base
import Data.Repa.Array.Material         as A
import Data.Repa.Array.Generic          as A
import Data.Repa.Array.Auto.IO          as A
import qualified Data.HashTable.IO      as Hash
import qualified System.Mem             as System
import Control.Monad
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
        -- Store an array of chunks for each file.
        -- We use a mutable vector of chunks, and store the number of used 
        -- slots in that vector separately.
        (ht :: Hash.CuckooHashTable FilePath (Int, M.IOVector (Array F Word8)))
         <- Hash.newSized 1024

        !refSize   <- newIORef 0
        !refChunks <- newIORef 0

        -- Flush the chunks for a single file to disk.
        let flush_path (path, (n, mvec))
             = do 
                  !vec   <- V.unsafeFreeze mvec

                  !h     <- openBinaryFile path AppendMode 

                  -- Write out chunks for this file.
                  V.mapM_ (hPutArray h . convert A) 
                        $ V.slice 0 n vec

                  hClose h

                  -- Delete the entry from the hash table.
                  -- This allows the space for the mutable vector of chunks to be reclaimed.
                  Hash.delete ht path


        -- Flush all the chunks we have stored.
        let flush_all 
             = do Hash.mapM_ flush_path ht

        -- Remember that we've accumulated this chunk into memory.
        -- When we end up with too much data then we flush the whole lot
        -- to the file system.
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

        -- Accept a single incoming element.
        let push_sieve _ !e

             = case diag e of

                -- The provided diag function told us to drop this
                -- element on the floor.
                Nothing 
                 -> return ()

                -- Accumulate a new chunk.
                Just (path, arr)
                 -> do  
                        -- See if we already have a buffer for this file.
                        !mElem   <- Hash.lookup ht path
                        case mElem of

                         -- We haven't seen chunks for this file before, 
                         -- so create a new vector to hold them.
                         Nothing
                          -> do !mvec    <- M.new 256
                                M.write mvec 0 arr
                                Hash.insert ht path (1, mvec)
                                acc_size (A.length arr)

                         -- We already have a chunk vector for this file.
                         Just (n, mvec)
                          -> do 
                                -- If the chunk vector has no space the expand it.
                                !mvec'  <- if n >= M.length mvec
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

