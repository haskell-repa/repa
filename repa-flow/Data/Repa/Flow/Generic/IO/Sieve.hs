
module Data.Repa.Flow.Generic.IO.Sieve
        (sieve_o)
where
import Data.Repa.Flow.Generic.Base
import Data.Repa.Array                  as A
import Data.Repa.Array.Material         as A
import Data.Repa.IO.Array               as A
import System.IO
import Data.Word
#include "repa-flow.h"


-- | Create an output sieve that writes data to an indeterminate number of
--   output files. Each new element is appended to its associated file.
--
--   * TODO: 
--     This function keeps a maximum of 8 files open at once, closing
--     and re-opening them in a least-recently-used order.
--     Due to this behaviour it's fine to create thousands of separate
--     output files without risking overflowing the process limit on 
--     the maximum number of useable file handles.
--
sieve_o :: (a -> Maybe (FilePath, Array F Word8))   
                                -- ^ Produce the desired file path and output
                                --   record for this element, or `Nothing` if
                                --   it should be discarded.
        -> IO (Sinks () IO a)

sieve_o diag
 = do

        let push_sieve _ e
             = case diag e of
                Nothing 
                 -> return ()

                Just (path, arr)
                 -> do  -- TODO: repeatededly opening and closing the file 
                        --       will be very slow.
                        h       <- openBinaryFile path AppendMode
                        hPutArray h arr
                        hClose h

        -- TODO: ignore any more incoming data after being ejected.
        let eject_sieve _ 
             = return ()

        return  $ Sinks () push_sieve eject_sieve
{-# INLINE sieve_o #-}
