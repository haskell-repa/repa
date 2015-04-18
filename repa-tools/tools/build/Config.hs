
module Config where
import Data.Maybe
import Prelude          as P


-- | Command-line configuration.
data Config
        = Config
        { -- | Major mode of program.
          configMode            :: Mode

          -- | Query source code, 
          --   can be the Haskell EDSL, or a JSON operator graph.
        , configQuery           :: Maybe FilePath }


-- | Starting configuration.
configZero :: Config
configZero
        = Config
        { configMode            = ModeBuild
        , configQuery           = Nothing }


-- | Major mode of program.
data Mode
        -- | Build a query to an executable.
        = ModeBuild

        -- | Emit operator graph in Haskell syntax.
        | ModeDumpGraph

        -- | Emit operator graph is JSON syntax.
        | ModeDumpJSON
        deriving (Eq, Show)


-- | Parse command-line arguments into a configuration.
parseArgs :: [String] -> Config -> IO Config

parseArgs [] config
 | isJust $ configQuery config
 = return config

 | otherwise    = dieUsage

parseArgs args config
 | "-query" : file : rest  <- args
 , Nothing                 <- configQuery config
 = parseArgs rest $ config { configQuery = Just file }

 | "-dump-graph" : rest      <- args
 = parseArgs rest $ config { configMode  = ModeToGraph }

 | "-dump-json"  : rest      <- args
 = parseArgs rest $ config { configMode  = ModeToJSON }

 | file : rest             <- args
 , x : _                   <- file
 , x /= '-'
 , Nothing                 <- configQuery config
 = parseArgs rest $ config { configQuery = Just file }

 | otherwise
 = dieUsage


-- | Die on wrong usage at command line.
dieUsage
 = error $ P.unlines
 [ "Usage: build -query FILE [OPTIONS]"
 , "Compile a Repa query into an executable, or emit its operator graph."
 , ""
 , "OPTIONS:"
 , " -dump-graph        Dump operator graph in Haskell syntax."
 , " -dump-json         Dump operator graph in JSON syntax." ]

