
module Config where
import Data.Char
import Data.Maybe
import Prelude          as P


-- | Command-line configuration.
data Config
        = Config
        { configDrop    :: Int 
        , configFileIn  :: Maybe FilePath 
        , configFormat  :: Maybe Format }

data Format
        = FormatCSV
        | FormatTSV


-- | Starting configuration.
configZero :: Config
configZero
        = Config
        { configDrop    = 0
        , configFileIn  = Nothing 
        , configFormat  = Nothing }


-- | Parse command-line arguments into a configuration.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config 
 | isJust $ configFileIn config
 = return config

 | otherwise = dieUsage

parseArgs args config
 | "-drop" : sn : rest  <- args
 , all isDigit sn
 = parseArgs rest $ config { configDrop = read sn }

 | "-csv" : rest        <- args
 = parseArgs rest $ config { configFormat = Just FormatCSV }

 | "-tsv" : rest        <- args
 = parseArgs rest $ config { configFormat = Just FormatTSV }

 | [filePath] <- args
 = return $ config { configFileIn = Just filePath }

 | otherwise
 = dieUsage


-- | Die on wrong usage at the command line.
dieUsage
 = error $ P.unlines
 [ "Usage: flow-fields FORMAT [OPTIONS] <source_file>"
 , "Split a file into separate files, one for each column."
 , ""
 , "FORMAT:"
 , " -tsv               Input file contains tab-separated values."
 , " -csv               Input file contains comma-separated values."
 , ""
 , "OPTIONS:"
 , " -drop (n :: Nat)   Drop n lines from the front of the input file." ]


-- | Die if the lines do not have the same number of fields.
dieFields       
 = error "Lines do not have the same number of fields."

