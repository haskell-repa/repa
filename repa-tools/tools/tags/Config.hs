
module Config where
import Data.Maybe
import System.Exit


-- | Command-line configuration.
data Config
        = Config
        { -- | Major mode.
          configMode            :: Maybe Mode

          -- | Input files to process.
        , configInFiles         :: [FilePath]

          -- | Directory to write output to.
        , configOutDir          :: Maybe FilePath 

          -- | Insert the given attribute into each row of output.
        , configInsertAttrs     :: [(String, String)] }


-- | Major mode.
data Mode
        = ModeUnpackRows


-- | Starting configuration.
configZero :: Config
configZero
        = Config
        { configMode            = Nothing
        , configInFiles         = []
        , configOutDir          = Nothing 
        , configInsertAttrs     = [] }


-- | Parse command-line arguments into a configuration.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config
 | isJust (configMode config) 
 , not $ null $ configInFiles config
 = return config

 | otherwise = dieUsage

parseArgs args config
 | "-unpack-rows" : rest <- args
 = parseArgs rest
 $ config { configMode    = Just ModeUnpackRows}

 | "-out-dir" : dir : rest <- args
 = parseArgs rest
 $ config { configOutDir  = Just dir}

 | "-insert-attr" : name : value : rest <- args
 = parseArgs rest
 $ config { configInsertAttrs = (configInsertAttrs config) ++ [(name, value)] }

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
         , "    -out-dir     PATH        Write output buckets to this directory (default '.')" 
         , "    -insert-attr NAME VALUE  Insert attribute into each output row (eg, a timestamp)"
         ]

        _ <- exitWith $ ExitFailure 1
        error "dieUsage"
