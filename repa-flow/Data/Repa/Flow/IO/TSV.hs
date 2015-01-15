
module Data.Repa.Flow.IO.TSV
        ( sourceTSV
        , hSourceTSV)
where
import Data.Repa.Flow
import Data.Repa.Array                  as A hiding (fromList, fromLists)
import qualified Data.Repa.Flow.Generic as G hiding (next)
import System.IO
import Data.Char


lix :: [a] -> Int -> Maybe a
lix (x : _)  0  = Just x
lix (_ : xs) n  = lix xs (n - 1)
lix _        _  = Nothing


-- | Read complete rows of data from an ASCII tab-separated-variable file.
sourceTSV
        :: [FilePath]           -- ^ File paths.
        -> Int                  -- ^ Chunk length.
        -> IO ()                -- ^ Action to perform if we find line longer
                                --   than the chunk length.
        -> IO (Sources UN (Vector UN (Vector F Char)))

sourceTSV filePaths len aFail
 = do   hs     <- mapM (flip openBinaryFile ReadMode) filePaths
        s0      <- hSourceTSV hs len aFail
        finalize_i (\i -> let Just h = lix hs i
                          in  hClose h) s0
{-# INLINE [2] sourceTSV #-}


-- | Like `sourceTSV` but take existing file handles.
hSourceTSV
        :: [Handle]             --  File paths.
        -> Int                  --  Chunk length.
        -> IO ()                --  Action to perform if we find line longer
                                --  than the chunk length.
        -> IO (Sources UN (Vector UN (Vector F Char)))

hSourceTSV hs nChunk aFail
 = do
        -- Rows are separated by new lines, 
        -- fields are separated by tabs.
        let !nl  = fromIntegral $ ord '\n'
        let !nr  = fromIntegral $ ord '\r'
        let !nt  = fromIntegral $ ord '\t'

        -- Stream chunks of data from the input file, where the chunks end
        -- cleanly at line boundaries. 
        sChunk  <- G.hSourcesChunks hs nChunk (== nl) aFail

        -- Dice the chunks of data into arrays of lines and fields.
        let isWhite c = c == nl || c == nr || c == nt
            {-# INLINE isWhite #-}

        sRows8  <- mapChunks_i 
                        (A.mapElems (A.trimEnds isWhite) . A.diceOn nt nl) 
                        sChunk

        -- Convert element data from Word8 to Char.
        -- Chars take 4 bytes each, but are standard Haskell and pretty
        -- print properly. We've done the dicing on the smaller Word8
        -- version, and now map across the elements vector in the array
        -- to do the conversion.
        sRows   
         <- mapChunks_i 
                (A.mapElems 
                        (A.mapElems 
                                (A.computeS F . A.map (chr . fromIntegral))))
                sRows8

        return sRows

{-# INLINE [2] hSourceTSV #-}

