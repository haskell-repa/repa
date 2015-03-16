
module Config where
import System.Exit
import Data.Char

-- | Command-line configuration.
data Config
        = Config
        { -- | Input files to sieve.
          configInFiles         :: [FilePath] 

          -- | Directory to write output buckets to,
          --   or `Nothing` to write to the current directory.
        , configOutDir          :: Maybe FilePath 

          -- | Index, name, and value of extra columns to insert in the output.
        , configInsertColumn    :: [(Int, String, String)] }


-- | Starting configuration.
configZero :: Config
configZero
        = Config
        { configInFiles         = [] 
        , configOutDir          = Nothing 
        , configInsertColumn    = [] }


-- | Parse command-line arguments into a configuration.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config 
 | not $ null $ configInFiles config
 = return config

 | otherwise = dieUsage

parseArgs args config
 | "-out-dir" : dir : rest <- args
 = parseArgs rest
 $ config { configOutDir  = Just dir}

 | "-insert-column" : spec : rest <- args
 , (sNum,  (':' : spec2))  <- break (== ':') spec
 , (sName, (':' : sValue)) <- break (== ':') spec2
 , all isDigit sNum
 = parseArgs rest
 $ config { configInsertColumn 
                = configInsertColumn config
                ++ [(read sNum, sName, sValue)] }

 | filePath : rest       <- args
 , c : _                 <- filePath
 , c /= '-'
 = parseArgs rest
 $ config { configInFiles = (configInFiles config) ++ [filePath] }

 | otherwise
 = dieUsage


-- | Die on wrong usage at the command line.
dieUsage :: IO a
dieUsage
 = do   putStrLn $ unlines
         [ "Usage: repa-sieve [OPTIONS] <source_file>"
         , ""
         , "OPTIONS:"
         , "  -out-dir PATH                    Write output buckets to this directory (default '.')"
         , "  -insert-column  NUM:NAME:VALUE   Insert a new column into the output"
         , ""
         , "SYNPOSIS:"
         , "  Split a .csv or .tsv file row-wise, based on the first field."
         , ""
         , "  Given a file containing:"
         , ""
         , "    key1,foo1,bar1"
         , "    key1,foo2,bar2"
         , "    key2,foo3,bar3"
         , "    key3,foo4,bar4"
         , "    key2,foo5,bar5"
         , ""
         , "  Sieving it yields three output files:"
         , ""
         , "    key1.csv          key2.csv          key3.csv"
         , "    ~~~~~~~~          ~~~~~~~~          ~~~~~~~~"
         , "    key1,foo1,bar1    key2,foo3,bar3    key3,foo4,bar4"
         , "    key1,foo2,bar2    key2,foo5,bar5"]

        _ <- exitWith $ ExitFailure 1
        error "dieUsage"

