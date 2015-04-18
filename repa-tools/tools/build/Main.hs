
import Config
import Data.Repa.Query.Convert.JSON                     ()
import Data.Repa.Query.Build                            as R
import Data.Repa.Query.Graph                            as R

import BuildBox.Build
import System.FilePath
import System.Environment
import qualified BuildBox.Build                         as BB
import qualified BuildBox.Command.System                as BB
import qualified Data.Aeson                             as Aeson
import qualified Data.ByteString.Lazy.Char8             as BS


main
 = do   args    <- getArgs
        config  <- parseArgs args configZero
        build config


build config
 = do   let Just file   = configQuery config
        result          <- runBuild "/tmp" (build_query config file)
        case result of
         Left err -> error $ show err
         Right _  -> return ()

build_query config file
 = case takeExtension file of
        ".hs"   
         -> do  dslQuery  <- BB.io $ readFile file
                graph     <- R.buildDslViaRepa "."  dslQuery  (dropExtension file)
                build_print config graph

        ".json" 
         -> do  jsonQuery <- BB.io $ readFile file
                graph     <- R.buildJsonViaRepa "." jsonQuery (dropExtension file)
                build_print config graph

build_print config graph
 = case configMode config of
        ModeBuild       
         -> return ()

        ModeToGraph
         -> do  io $ putStrLn    $ show graph
                return ()

        ModeToJSON
         -> do  io $ BS.putStrLn $ (Aeson.encode $ Aeson.toJSON graph)
                return ()
