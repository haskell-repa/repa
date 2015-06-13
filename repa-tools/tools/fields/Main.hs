import Config
import Data.Repa.Flow.Auto.IO                   as F
import Data.Repa.Array                          as A
import qualified Data.Repa.Flow.Generic         as G
import qualified Data.Repa.Flow.Generic.IO      as G

import Control.Concurrent
import Control.Monad
import System.Environment
import System.IO
import Data.Maybe
import Data.Char
import Prelude                                  as P

main :: IO ()
main 
 = do   args    <- getArgs
        config  <- parseArgs args configZero
        pFields config


pFields :: Config -> IO ()
pFields config
 = do   
        -- Open the file and look at the first line to see how many
        -- fields there are.
        let Just fileIn = configFileIn config
        hIn       <- openFile fileIn ReadMode
        strFirst  <- hGetLine hIn

        -- TODO: Avoid sketchy replacement of commas by tabs.
        --       Check how CSV standard escapes tab characters.
        let Just format = configFormat config
        let cols  
                =  case format of
                    FormatTSV -> P.length $ words strFirst
                    FormatCSV -> P.length $ words
                               $ (P.map (\c -> if c == ',' then '\t' else c) 
                                        strFirst)

        hSeek hIn AbsoluteSeek 0

        -- Drop the requested number of lines from the front,
        -- to find out the starting position.
        mapM_ (\_ -> hGetLine hIn) [1 .. configDrop config]
        pStart    <- hTell hIn
        hClose hIn

        -- Reopen the file using the same number of buckets
        -- as we have Haskell capabilities.
        nCaps      <- getNumCapabilities 
        let !nl    =  fromIntegral $ ord '\n'

        sIn        <- fromSplitFileAt nCaps (== nl) fileIn pStart 
                   $  case format of
                        FormatTSV -> sourceTSV
                        FormatCSV -> sourceCSV

        -- Do a ragged transpose the chunks, to produce a columnar
        -- representation.
        sColumns   <- G.map_i ragspose3 sIn

        -- Concatenate the fields in each column.
        sCat       <- G.map_i (A.map A.unlines) sColumns

        -- Open an output directory for each of the columns.
        let dirsOut = [fileIn ++ "." ++ show n | n <- [0 .. cols - 1]]
        ooOut      <- G.toDirs nCaps dirsOut $ G.sinkChars

        -- Chunks are distributed into each of the output files.
        -- Die if we find a row that has more fields than the first one.
        ooOut'     <- G.flipIndex2_o  ooOut
        oOut       <- G.distribute2_o dieFields ooOut' 

        -- Drain all the input chunks into the output files,
        -- processing each of the input buckets in parallel.
        G.drainP sCat oOut

