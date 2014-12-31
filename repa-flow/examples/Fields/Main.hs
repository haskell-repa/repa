
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
import Data.Repa.Flow.Chunked
import Data.Repa.Flow.Chunked.IO        
import System.Environment
import System.IO
import Control.Monad
import Data.Char
import Data.Word
import qualified Data.Repa.Flow.Simple          as S
import qualified Data.Repa.Flow.Generic         as G
import Data.Repa.Array                          as R
import Data.Repa.Array.Foreign                  as R
import Prelude                                  as P


main :: IO ()
main 
 = do   args    <- getArgs
        config  <- parseArgs args configZero
        pFields config

pFields :: Config -> IO ()
pFields config
 = do   
        -- Open the file and drop the requested number of lines from
        -- the front of it.
        let Just fileIn = configFileIn config
        hIn     <- openFile fileIn ReadMode
        _       <- mapM (\_ -> hGetLine hIn) [1 .. configDrop config]

        -- Rows are separated by new lines, 
        -- fields are separated by tabs.
        let !nl  = fromIntegral $ ord '\n'
        let !nr  = fromIntegral $ ord '\r'
        let !nt  = fromIntegral $ ord '\t'
        let !(fl  :: Vector F Word8) = R.vfromList [nl]

        -- Stream chunks of data from the input file, where the chunks end
        -- cleanly at line boundaries. 
        -- Error out if we read a whole chunk that does not contain
        -- end-of-line characters.
        sIn     ::  Sources () IO F Word8
                <-  G.project_i (zero 1)
                =<< finalize_i (\_ -> hClose hIn)
                =<< hSourcesRecords [hIn]
                        (64 * 1024) (== nl)
                        (error "Found an over-long line")
 
        -- Dice the chunks of data into arrays of lines and fields.
        let isWhite c = c == nl || c == nr || c == nt
            {-# INLINE isWhite #-}

        sFields   <- mapChunks_i (R.maps (R.trimEnds isWhite) . diceOn nt nl) sIn

        -- Do a ragged transpose the chunks, so we get a columnar representation.
        sColumns  <- mapChunks_i ragspose3 sFields

        -- Concatenate the fields in each column.
        sColumnsC :: Sources () IO B (Vector F Word8)
                  <- mapChunks_i (R.computeS . R.map (R.concatWith fl)) sColumns

        -- Peek at the first chunk to see how many columns we have.
        ([k1], sColumnsC) <- S.peek_i 1 sColumnsC
        let cols  = R.length k1

        -- Open an output file for each of the columns.
        let filesOut = [fileIn ++ "." ++ show n | n <- [0 .. cols - 1]]
        ooOut     <- fileSinksBytes filesOut

        -- Chunks are distributed into each of the output files.
        -- Error out if we find a row that has more fields than the first
        -- one did.
        oOut      <- G.distributes_o ooOut 
                        (error "spilled field")

        -- Drain all the input chunks into the output files.
        drain sColumnsC oOut


---------------------------------------------------------------------------------------------------
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


