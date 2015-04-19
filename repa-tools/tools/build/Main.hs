
-- | Compile a Repa query to an executable.
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
 = do   let Just file   =  configQuery config
        result          <- runBuild "/tmp" (build_mode config (configMode config) file)
        case result of
         Left err -> error $ show err
         Right _  -> return ()


build_mode  config mode file
 = case mode of
        ModeBuild       -> build_build   config file
        ModeToGraph     -> build_toGraph config file
        ModeToJSON      -> build_toJSON  config file


build_build config file
 = case takeExtension file of
        ".hs"   
         -> do  dslQuery  <- BB.io $ readFile file
                _graph    <- R.buildDslViaRepa 
                                "." (not $ configDump config)
                                dslQuery  (dropExtension file)
                return ()

        ".json" 
         -> do  jsonQuery <- BB.io $ readFile file
                _graph    <- R.buildJsonViaRepa 
                                "." (not $ configDump config)
                                jsonQuery (dropExtension file)
                return ()

        ext -> error $ "Cannot compile a " ++ ext ++ " file to a query."


build_toGraph config file
 = case takeExtension file of
        ".hs"
         -> do  dslQuery  <- BB.io $ readFile file
                graph     <- R.loadQueryFromDSL 
                                (configDirScratch config) (not $ configDump config)
                                dslQuery 
                io $ putStrLn $ show graph
                return ()

        ".json"
         -> do  jsonQuery <- BB.io $ readFile file
                graph     <- R.loadQueryFromJSON
                                (configDirScratch config) (not $ configDump config)
                                jsonQuery
                io $ putStrLn $ show graph

        ext -> error $ "Cannot convert a " ++ ext ++ " file to a query."


build_toJSON config file
 = case takeExtension file of
        ".hs"
         -> do  dslQuery  <- BB.io $ readFile file
                graph     <- R.loadQueryFromDSL 
                                (configDirScratch config) (not $ configDump config)
                                dslQuery 
                io $ BS.putStrLn $ Aeson.encode $ Aeson.toJSON graph
                return ()

        ".json"
         -> do  jsonQuery <- BB.io $ readFile file
                graph     <- R.loadQueryFromJSON
                                (configDirScratch config) (not $ configDump config)
                                jsonQuery
                io $ BS.putStrLn $ Aeson.encode $ Aeson.toJSON graph

        ext -> error $ "Cannot convert a " ++ ext ++ " file to a JSON query."


