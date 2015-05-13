
module Config where
import System.Exit


-- | Command-line configuration.
data Config
        = Config
        { -- | Major mode.
          configMode            :: Maybe Mode

          -- | Input files to process.
        , configInFiles         :: [FilePath]

          -- | Directory to write output to.
        , configOutDir          :: Maybe FilePath }


-- | Major mode.
data Mode
        = ModeUnpackRows


-- | Starting configuration.
configZero :: Config
configZero
        = Config
        { configMode            = Nothing
        , configInFiles         = []
        , configOutDir          = Nothing }


-- | Parse command-line arguments into a configuration.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config
 | not $ null $ configInFiles config
 = return config

 | otherwise = dieUsage

parseArgs args config
 | "-unpack-rows" : rest <- args
 = parseArgs rest
 $ config { configMode    = Just ModeUnpackRows}

 | "-out-dir" : dir : rest <- args
 = parseArgs rest
 $ config { configOutDir  = Just dir}

 | filePath : rest       <- args
 , c : _                 <- filePath
 , c /= '-'
 = parseArgs rest
 $ config { configInFiles = (configInFiles config) ++ [filePath] }

 | otherwise
 = do   putStrLn $ "repa-tags: no input files."
        dieUsage


-- | Die on wrong usage at the command line.
dieUsage :: IO a
dieUsage
 = do   putStrLn $ unlines
         [ "XML tag tools."
         , ""
         , " Unpack rows from an XML file."
         , "  repa-tags -unpack-rows <FILE.xml> ..."
         , "  Options:"
         , "    -out-dir PATH     Write output buckets to this directory (default '.')" 
         ]

        _ <- exitWith $ ExitFailure 1
        error "dieUsage"
