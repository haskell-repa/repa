
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
import Data.Repa.Flow
import Data.Repa.Flow.IO
import Data.Repa.Flow.IO.TSV
import Data.Repa.Array                          as A
import qualified Data.Repa.Flow.Generic         as G

import Control.Monad
import System.Environment
import System.IO
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
        let cols  =  P.length $ words strFirst
        hSeek hIn AbsoluteSeek 0

        -- Drop the requested number of lines from the front.
        mapM_ (\_ -> hGetLine hIn) [1 .. configDrop config]

        -- Stream the rest of the file as TSV.
        sIn       <- sourceTSV lenChunk dieLong [hIn]

        -- Do a ragged transpose the chunks, to produce a 
        -- columnar representation.
        sColumns  <- mapChunks_i ragspose3 sIn

        -- Concatenate the fields in each column.
        let !fl   =  A.vfromList F ['\n']
        sCat      <- mapChunks_i (A.computeS B . A.map (A.concatWith F fl))
                                 sColumns

        -- Open an output file for each of the columns.
        let filesOut = [fileIn ++ "." ++ show n | n <- [0 .. cols - 1]]
        ooOut     <- toFiles filesOut $ sinkChars

        -- Chunks are distributed into each of the output files.
        -- Die if we find a row that has more fields than the first one.
        oOut      <- G.distributes_o ooOut dieFields

        -- Drain all the input chunks into the output files.
        sSingle   <- G.project_i (IIx 0 1) sCat
        G.drain sSingle oOut


-------------------------------------------------------------------------------
data Config
        = Config
        { configDrop    :: Int 
        , configFileIn  :: Maybe FilePath }

configZero
        = Config
        { configDrop    = 0
        , configFileIn  = Nothing }

parseArgs :: [String] -> Config -> IO Config
parseArgs [] config 
        = return config

parseArgs args config
 | "-drop" : sn : rest <- args
 , all isDigit sn
 = parseArgs rest $ config { configDrop = read sn }

 | [filePath] <- args
 = return $ config { configFileIn = Just filePath }

 | otherwise
 = error $ unlines
 [ "Usage: flow-fields [OPTIONS] <source_file>"
 , "Split a TSV file into separate files, one for each column."
 , ""
 , " -drop (n :: Nat)   Drop n lines from the front of the input file." ]


-- | Default chunk length, in bytes.
lenChunk        = 1024

-- | Die if we find a line longer than a chunk.
dieLong         = error "Found over-long line"

-- | Die if the lines do not have the same number of fields.
dieFields       = error "Lines do not have the same number of fields."

