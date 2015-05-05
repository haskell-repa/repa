
module Data.Repa.Query.Build
        ( -- * Config
          QB.Config (..)

          -- * Building
        , buildJobViaRepa 
        , buildJsonViaRepa
        , buildDslViaRepa

          -- * Loading
        , loadJobFromDSL
        , loadJobFromJSON)
where
import System.FilePath
import System.Directory
import Control.Monad
import qualified Data.Repa.Query.Build.Repa             as CR
import qualified Data.Repa.Query.Graph.JSON             ()
import qualified Data.Repa.Query.Job.Spec               as Q
import qualified Data.Repa.Query.Job.JSON               ()
import qualified Data.Repa.Query.Source.Builder         as QB
import qualified Data.Aeson                             as Aeson
import qualified Data.ByteString.Lazy.Char8             as BS
import qualified Data.List                              as L

import qualified Language.Haskell.TH                    as TH

import qualified BuildBox.Build                         as BB
import qualified BuildBox.Command.System                as BB
import qualified BuildBox.Command.File                  as BB


---------------------------------------------------------------------------------------------------
-- | Produce an executable for the given job.
--
--   We use Template Haskell to convert the job to Haskell using the
--   Repa Flow library, then compile it with GHC.
--
--   The working directory is used to store the intermediate Haskell
--   source file as well as the built executable. Written files are not
--   removed after the build, so the whole directory and its contents
--   should be removed by the caller when they're done.
--
buildJobViaRepa
        :: FilePath                     -- ^ Working directory.
        -> Bool                         -- ^ Cleanup intermediate files.
        -> Q.Job                        -- ^ Job to compile.
        -> FilePath                     -- ^ Path of output executable.
        -> BB.Build ()

buildJobViaRepa dirScratch cleanup job fileExe
 = do   
        -- Ensure the working directory already exists.
        BB.ensureDir dirScratch

        -- Use Template Haskell to convert the query into Haskell code.
        dec <- BB.io $ TH.runQ $ CR.decOfJob (TH.mkName "_runJob") job

        let fileHS      = dirScratch </> "Main.dump-repa.hs" -- written by us.
        let fileHI      = dirScratch </> "Main.dump-repa.hi" -- dropped by GHC
        let fileHO      = dirScratch </> "Main.dump-repa.o"  -- dropped by GHC

        -- Write out Haskell into a temp file.
        BB.io $ writeFile fileHS
              $ repaHeader ++ "\n" ++ TH.pprint dec ++ "\n\n"

        -- Call GHC to build the query.
        _  <- BB.sesystemq $ "ghc -fforce-recomp -O2 --make " ++ fileHS ++ " -o " ++ fileExe

        -- Remove dropped files.
        when cleanup
         $ BB.io $ mapM_ removeFile [fileHS, fileHI, fileHO]

        return ()


---------------------------------------------------------------------------------------------------
-- | Like `buildQueryViaRepa`, but accept a query encoded as JSON.
buildJsonViaRepa
        :: FilePath             -- ^ Working directory.
        -> Bool                 -- ^ Cleanup intermediate files.
        -> String               -- ^ Query encoded as JSON.
        -> FilePath             -- ^ Path to output executable.
        -> BB.Build Q.Job       -- ^ Operator graph for job.

buildJsonViaRepa dirScratch cleanup json fileExe
 = do   
        -- Parse the json version into a job.
        let Just job :: Maybe Q.Job
                = Aeson.decode $ BS.pack json

        -- Build query into an executable.
        buildJobViaRepa dirScratch cleanup job fileExe

        return  job


---------------------------------------------------------------------------------------------------
-- | Like `buildJobViaRepa`, but accept a job encoded in the DSL.
buildDslViaRepa
        :: FilePath             -- ^ Working directory.
        -> Bool                 -- ^ Cleanup intermediate files.
        -> QB.Config            -- ^ Query builder config.
        -> String               -- ^ Query encoded in the DSL.
        -> FilePath             -- ^ Path to output executable.
        -> BB.Build (Either String Q.Job)
                                -- ^ Operator graph of compiled query.

buildDslViaRepa dirScratch cleanup dslConfig dslJob fileExe
 = do   
        -- Load json from the dsl version of the job.
        eJob   <- loadJobFromDSL dirScratch cleanup dslConfig dslJob
        case eJob of
         Left  err   -> return $ Left err
         Right job
          -> do -- Build job into an executable.
                buildJobViaRepa dirScratch cleanup job fileExe
                return $ Right job


---------------------------------------------------------------------------------------------------
-- | Load the operator graph from a query encoded in the DSL.
loadJobFromDSL
        :: FilePath             -- ^ Working directory.
        -> Bool                 -- ^ Cleanup intermediate files.
        -> QB.Config            -- ^ Job builder config.
        -> String               -- ^ Job encoded in the DSL.
        -> BB.Build (Either String Q.Job)

loadJobFromDSL dirScratch cleanup dslConfig dslJob
 = do
        -- Attach header that defines the prims, and write out the code.
        --
        -- TODO: sanitize query before pasting on header to make sure it
        --       doesn't try to import other modules.
        --
        let fileHS      = dirScratch </> "Main.dump-dsl.hs"

        BB.io $ writeFile fileHS (edslHeader ++ dslJob)

        -- Run the job builder to get the JSON version.
        msg     <- BB.sesystemq 
                $ "ghc " ++ fileHS ++ " -e " ++ "\"" ++ edslBuild dslConfig ++ "\""

        -- Remove dropped files.
        when cleanup
         $ BB.io $ removeFile fileHS

        let result
                -- EDSL job builder successfully produced the JSON version.
                --
                -- In this case we get the string "OK:" followed by the pretty
                -- printed JSON.
                --
                | Just jsonJob       <- L.stripPrefix "OK:" msg
                = case Aeson.decode $ BS.pack jsonJob of
                     -- Sadly, the produced JSON could not be parsed.
                     Nothing
                      -> return $ Left $ "loadJobFromDSL: cannot parse produced JSON."

                     -- The produced JSON parsed ok.
                     Just (job :: Q.Job)
                      -> return $ Right job


                -- The EDSL query builder ran, but had some problem and couldn't
                -- produce the query. Maybe it couldn't find table meta-data, 
                -- or detected that the query was malformed.
                -- 
                -- In this case we get the string "FAIL:" followed by a 
                -- description of what went wrong.
                --
                | Just err              <- L.stripPrefix "FAIL:" msg
                =     return $ Left err

                -- The EDSL code itself could not be run by GHC. There is probably
                -- a syntax or type error in the EDSL code.
                --
                -- In this case we get an error message directly from GHC.
                -- 
                -- TODO: we only get here if the query prints some other junk
                -- GHC errors get caught by buildbox instead.
                -- 
                | otherwise
                =     return $ Left $ "loadQueryFromDSL: cannot parse result"

        result


---------------------------------------------------------------------------------------------------
-- | Load a job description from JSON.
loadJobFromJSON
        :: FilePath             -- ^ Working directory.
        -> Bool                 -- ^ Cleanup intermediate files.
        -> String               -- ^ Query encoded in the DSL.
        -> BB.Build Q.Job

loadJobFromJSON _dirScratch _cleanup jsonJob
 = do
        let Just job :: Maybe Q.Job
                = Aeson.decode $ BS.pack jsonJob

        return job

---------------------------------------------------------------------------------------------------
-- | Junk pasted to the front of a query written in the EDSL 
--   to make it a well formed Haskell program.
edslHeader :: String
edslHeader
 = unlines
 [ "{-# LANGUAGE NoImplicitPrelude   #-}"
 , "{-# LANGUAGE ScopedTypeVariables #-}"
 , "{-# LANGUAGE GADTs               #-}"
 , "import Data.Repa.Query.Source"
 , "import qualified Data.Repa.Query.Source.Builder     as Q"
 , "import qualified Data.Repa.Query.Graph.JSON         as Q"
 , "import qualified Data.ByteString.Lazy               as B (append)"
 , "import qualified Data.ByteString.Lazy.Char8         as B (putStrLn, pack)"
 , "import qualified Data.Aeson                         as A (encode,   toJSON)"
 , "import qualified Data.Either                        as E (Either(..))"
 , "import qualified Prelude                            as P (show)"
 , ""]

-- TODO: lift this goop out into a separate wrapper module.
edslBuild :: QB.Config -> String
edslBuild config
 =  "Q.runQ " ++ edslConfig config ++ " result "
 ++ ">>= \\r -> case r of { "
 ++ "   E.Left  err   -> B.putStrLn (B.append (B.pack \\\"FAIL:\\\") (B.pack (P.show err)));"
 ++ "   E.Right graph -> B.putStrLn (B.append (B.pack \\\"OK:\\\")   (A.encode (A.toJSON graph)))"
 ++ "}"


edslConfig :: QB.Config -> String
edslConfig config
        =  "Q.Config { "
        ++ " Q.configRootData = " ++ "\\\"" ++ QB.configRootData config ++ "\\\""
        ++ " }"


---------------------------------------------------------------------------------------------------
-- | Junk pasted to the front of generated Repa code 
--   to make it a well formed Haskell program.
repaHeader :: String
repaHeader
 = unlines 
 [ "import qualified Data.Repa.Query.Runtime.Driver"
 , "import qualified Data.Repa.Query.Runtime.Primitive"
 , "import qualified Data.Repa.Convert.Formats"
 , "import qualified Data.Repa.Product"
 , ""
 , "main = _runJob" ]



