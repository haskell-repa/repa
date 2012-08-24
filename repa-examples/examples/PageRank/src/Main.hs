
import External.Count
import External.Rank
import Internal.Rank
import System.Environment
import System.Exit
import Data.Char

-- | Command-line help.
help :: String
help
 = unlines 
 [ "pagerank -count <LINKS_FILE>"
 , "pagerank [-steps <INT>] -rank-external <LINKS_FILE>"
 , "pagerank [-steps <INT>] -rank-internal <LINKS_FILE> <TITLES_FILE> [+RTS -N<THREADS>]" ]


-- | Command line configuration.
data Config
        = Config
        { configMode    :: Mode
        , configSteps   :: Int }

-- | Program mode.
data Mode
        = ModeNone

        -- | Count the total number of pages in a links file.
        | ModeCount        FilePath

        -- | Run the external algorithm,
        --   given the links and titles files.
        | ModeRankExternal FilePath FilePath

        -- | Run the internal algorithm,
        --   given the links and titles files.
        | ModeRankInternal FilePath FilePath


-- | Default program configuration.
defaultConfig :: Config
defaultConfig
        = Config
        { configMode    = ModeNone
        , configSteps   = 10 }


-- | Parse command line arguments.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config
        = return config

parseArgs args config
        | "-count" : linksPath : rest      <- args
        = parseArgs rest
        $ config { configMode = ModeCount linksPath }

        | "-rank-external" : pagesPath : titlesPath : rest <- args
        = parseArgs rest
        $ config { configMode = ModeRankExternal pagesPath titlesPath }

        | "-rank-internal" : pagesPath : titlesPath : rest <- args
        = parseArgs rest
        $ config { configMode = ModeRankInternal pagesPath titlesPath }

        | "-steps" : count : rest <- args
        , all isDigit count
        = parseArgs rest
        $ config { configSteps = read count }

        | otherwise
        = do    putStrLn help
                exitWith ExitSuccess


main :: IO ()
main
 = do   args    <- getArgs
        config  <- parseArgs args defaultConfig

        case configMode config of
         ModeNone
          -> do putStrLn help
                exitWith ExitSuccess

         ModeCount linksPath
          -> do _       <- countPages linksPath
                return ()

         ModeRankExternal linksPath titlesPath
          -> rankExternal (configSteps config) linksPath titlesPath

         ModeRankInternal linksPath titlesPath
          -> rankInternal (configSteps config) linksPath titlesPath

