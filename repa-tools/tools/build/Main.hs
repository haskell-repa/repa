
-- | Compile a Repa query to an executable.
import Config
import Data.Repa.Query.Graph.JSON                       ()
import Data.Repa.Query.Build                            as QB

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
 | ".hs"        <- takeExtension file
 = do   dslQuery  <- BB.io $ readFile file
        let Just pathRoot  = Config.configRootData config
        let dslConfig      = QB.Config pathRoot
        egraph    <- QB.buildDslViaRepa 
                        "." (not $ configDump config)
                         dslConfig dslQuery  (dropExtension file)
        checkResult egraph

 | ".json"      <- takeExtension file
 = do   jsonQuery <- BB.io $ readFile file
        graph     <- QB.buildJsonViaRepa 
                        "." (not $ configDump config)
                        jsonQuery (dropExtension file)
        checkResult (Right graph)

 | otherwise
 =      error $ "Cannot compile a " ++ (takeExtension file) ++ " file to a query."
 where
        checkResult (Left err)
         = error err

        checkResult (Right _graph)
         = return ()


---------------------------------------------------------------------------------------------------
build_toGraph config file
 -- Convert DSL query to graph AST.
 | ".hs"        <- takeExtension file
 = do   dslQuery  <- BB.io $ readFile file
        let Just pathRoot  = Config.configRootData config
        let dslConfig      = QB.Config pathRoot

        egraph    <- QB.loadJobFromDSL 
                        (configDirScratch config) (not $ configDump config)
                        dslConfig dslQuery 
        printErrGraph egraph

 -- Convert JSON query to graph ASt.
 | ".json"      <- takeExtension file
 = do   jsonQuery <- BB.io $ readFile file
        graph     <- QB.loadJobFromJSON
                        (configDirScratch config) (not $ configDump config)
                        jsonQuery
        printErrGraph (Right graph)

 | otherwise
 =      error $ "Cannot convert a " ++ takeExtension file ++ " file to a query."

 where  printErrGraph (Left err)
         = error err

        printErrGraph (Right graph)
         = io $ putStrLn $ show graph


---------------------------------------------------------------------------------------------------
build_toJSON config file
 -- Convert DSL query to json.
 | ".hs"        <- takeExtension file
 = do   dslQuery           <- BB.io $ readFile file
        let Just pathRoot  = Config.configRootData config
        let dslConfig      = QB.Config pathRoot

        egraph  <- QB.loadJobFromDSL 
                        (configDirScratch config) (not $ configDump config)
                        dslConfig dslQuery 

        printErrJSON egraph

 -- Load JSON query and print it back.
 | ".json"      <- takeExtension file
 = do   jsonQuery <- BB.io $ readFile file
        graph     <- QB.loadJobFromJSON
                        (configDirScratch config) (not $ configDump config)
                        jsonQuery

        printErrJSON (Right graph)

 | otherwise
 =      error $ "Cannot convert a " ++ takeExtension file ++ " file to a JSON query."

 where  printErrJSON (Left err)
         = error err

        printErrJSON (Right graph)
         = io $ BS.putStrLn 
              $ Aeson.encodePretty' aesonConfig
              $ Aeson.toJSON graph


---------------------------------------------------------------------------------------------------
aesonConfig
 = Aeson.defConfig
        { Aeson.confIndent      = 4
        , Aeson.confCompare     = compare }




