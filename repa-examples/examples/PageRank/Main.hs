{-# LANGUAGE BangPatterns #-}

import External.Count
import External.Rank
import Internal.Rank
import System.Environment
import Data.Char


data Config
        = Config
        { configMode    :: Mode
        , configSteps   :: Int }

data Mode
        = ModeNone
        | ModeCount        FilePath
        | ModeRankExternal FilePath FilePath
        | ModeRankInternal FilePath FilePath

defaultConfig :: Config
defaultConfig
        = Config
        { configMode    = ModeNone
        , configSteps   = 10 }

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
        = error "usage"


main :: IO ()
main
 = do   args    <- getArgs
        config  <- parseArgs args defaultConfig

        case configMode config of
         ModeNone
          -> do putStrLn "Nothing to do..."
                return ()

         ModeCount linksPath
          -> do _       <- countPages linksPath
                return ()

         ModeRankExternal linksPath titlesPath
          -> rankExternal (configSteps config) linksPath titlesPath

         ModeRankInternal linksPath titlesPath
          -> rankInternal (configSteps config) linksPath titlesPath

