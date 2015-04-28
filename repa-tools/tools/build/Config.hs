
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

          -- | Root directory containing meta data for tables.
        , configRootData        :: Maybe FilePath

          -- | Dump intermediate files.
        , configDump            :: Bool 

          -- | Scratch directory to use for compilation.
        , configDirScratch      :: FilePath }


-- | Starting configuration.
configZero :: Config
configZero
        = Config
        { configMode            = ModeBuild
        , configQuery           = Nothing
        , configRootData        = Just "." 
        , configDump            = False 
        , configDirScratch      = "." }


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
 | isJust $ configQuery    config
 , isJust $ configRootData config
 = return config

 | otherwise    = dieUsage

parseArgs args config
 | "-query" : file : rest  <- args
 , Nothing                 <- configQuery config
 = parseArgs rest $ config { configQuery    = Just file }

 | "-root-data" : path : rest  <- args
 = parseArgs rest $ config { configRootData = Just path }

 | "-dump"  : rest         <- args
 = parseArgs rest $ config { configDump     = True }

 | "-to-graph" : rest      <- args
 = parseArgs rest $ config { configMode     = ModeToGraph }

 | "-to-json"  : rest      <- args
 = parseArgs rest $ config { configMode     = ModeToJSON }

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
 [ "Usage: build -root-data PATH FILE  [OPTIONS]"
 , "Compile a Repa query into an executable."
 , "The query can be written in either the query DSL, or in the JSON format."
 , ""
 , "OPTIONS:"
 , " -root-data PATH    (required) Root path containing table meta data."
 , " -dump              Dump intermediate files."
 , " -to-graph          Emit operator graph in Haskell syntax."
 , " -to-json           Emit operator graph in JSON syntax." ]

