
-- | Compile a Repa query to an executable.
import Config
import Data.Repa.Query.Convert.JSON                     ()
import Data.Repa.Query.Build                            as QB
import Data.Repa.Query.Graph                            as R

import BuildBox.Build
import System.FilePath
import System.Environment
import qualified BuildBox.Build                         as BB
import qualified Data.Aeson                             as Aeson
import qualified Data.Aeson.Encode.Pretty               as Aeson
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
                let Just pathRoot  = Config.configRoot config
                let dslConfig      = QB.Config pathRoot
                _graph    <- QB.buildDslViaRepa 
                                "." (not $ configDump config)
                                dslConfig dslQuery  (dropExtension file)
                return ()

        ".json" 
         -> do  jsonQuery <- BB.io $ readFile file
                _graph    <- QB.buildJsonViaRepa 
                                "." (not $ configDump config)
                                jsonQuery (dropExtension file)
                return ()

        ext -> error $ "Cannot compile a " ++ ext ++ " file to a query."


build_toGraph config file
 = case takeExtension file of
        ".hs"
         -> do  dslQuery  <- BB.io $ readFile file
                let Just pathRoot  = Config.configRoot config
                let dslConfig      = QB.Config pathRoot

                graph     <- QB.loadQueryFromDSL 
                                (configDirScratch config) (not $ configDump config)
                                dslConfig dslQuery 
                io $ putStrLn $ show graph
                return ()

        ".json"
         -> do  jsonQuery <- BB.io $ readFile file
                graph     <- QB.loadQueryFromJSON
                                (configDirScratch config) (not $ configDump config)
                                jsonQuery
                io $ putStrLn $ show graph

        ext -> error $ "Cannot convert a " ++ ext ++ " file to a query."


build_toJSON config file
 = case takeExtension file of
        ".hs"
         -> do  dslQuery           <- BB.io $ readFile file
                let Just pathRoot  = Config.configRoot config
                let dslConfig      = QB.Config pathRoot

                graph     <- QB.loadQueryFromDSL 
                                (configDirScratch config) (not $ configDump config)
                                dslConfig dslQuery 

                io $ BS.putStrLn 
                        $ Aeson.encodePretty' aesonConfig
                        $ Aeson.toJSON graph

                return ()

        ".json"
         -> do  jsonQuery <- BB.io $ readFile file

                graph     <- QB.loadQueryFromJSON
                                (configDirScratch config) (not $ configDump config)
                                jsonQuery

                io $ BS.putStrLn 
                        $ Aeson.encodePretty' aesonConfig
                        $ Aeson.toJSON graph

        ext -> error $ "Cannot convert a " ++ ext ++ " file to a JSON query."

aesonConfig
 = Aeson.defConfig
        { Aeson.confIndent      = 4
        , Aeson.confCompare     = compare }




