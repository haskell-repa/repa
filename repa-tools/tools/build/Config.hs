
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
        , configQuery           :: Maybe FilePath 

          -- | Dump intermediate files.
        , configDump            :: Bool }


-- | Starting configuration.
configZero :: Config
configZero
        = Config
        { configMode            = ModeBuild
        , configQuery           = Nothing 
        , configDump            = False }


-- | Major mode of program.
data Mode
        -- | Build a query to an executable.
        = ModeBuild

        -- | Emit operator graph in Haskell syntax.
        | ModeToGraph

        -- | Emit operator graph is JSON syntax.
        | ModeToJSON
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

 | "-dump" : rest            <- args
 = parseArgs rest $ config { configDump  = True }

 | "-to-graph" : rest      <- args
 = parseArgs rest $ config { configMode  = ModeToGraph }

 | "-to-json"  : rest      <- args
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
 , "Compile a Repa query into an executable."
 , "The query can be written in either the query DSL, or in the JSON format."
 , ""
 , "OPTIONS:"
 , " -dump              Dump intermediate files."
 , " -to-graph          Emit operator graph in Haskell syntax."
 , " -to-json           Emit operator graph in JSON syntax." ]

