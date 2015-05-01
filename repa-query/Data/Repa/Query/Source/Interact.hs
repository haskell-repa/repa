
-- | Driver commands for running queries interactively in GHCi.
module Data.Repa.Query.Source.Interact
        ( module Data.Repa.Query.Source
        , runWith)
--        , graphWith
--        , jsonWith
--        , repaWith)
where
import Data.Repa.Query.Source
import Data.Repa.Query.Job
import qualified Data.Repa.Query.Source.Builder         as Q

-- import qualified Data.Repa.Query.Source                 as Q
import qualified Data.Repa.Query.Build                  as Q
-- import qualified Data.Repa.Query.Build.Repa             as Q

-- import qualified Data.Aeson                             as Aeson
-- import qualified Data.Aeson.Encode.Pretty               as Aeson

-- import qualified Data.ByteString.Lazy.Char8             as BS

import qualified System.Process                         as S

-- import qualified Language.Haskell.TH                    as TH

import qualified BuildBox                               as BB
import qualified BuildBox.Command.File                  as BB
-- import Prelude                                          as P


---------------------------------------------------------------------------------------------------
-- | Run a query locally, and print the output to stdout.
runWith :: FilePath                             -- ^ Scratch path for temp files.
        -> FilePath                             -- ^ Root path to data.
        -> (Q.Config -> IO (Either String [Job])) -- ^ Job builder.
        -> IO ()

runWith pathScratch pathRootData mkJobs
 = do   
        -- Execute the job builder to get a list of jobs to run.
        -- The builder might fail with some error if the job description
        -- is malformed, or it can't find appropriate meta-data.
        let config  = Q.Config pathRootData
        eJobs      <- mkJobs config

        case eJobs of
         Left err   
          -> error (show err)

         Right jobs 
          -> do result     <- BB.runBuild pathScratch (runJobs jobs)
                case result of
                 Left err -> error (show err)
                 Right x  -> return x

 where
        runJobs []      = return ()
        runJobs (j:js)  = runJob j >> runJobs js

        runJob (JobQuery q _outputFormat)   -- TODO: pass along outputFormat
         = BB.withTempFile $ \pathExe
         -> do  Q.buildQueryViaRepa 
                        pathScratch True
                        q pathExe

                BB.io $ S.system 
                      $ pathExe ++ " -root-data " ++ pathRootData

        runJob _
         = error "runJob: finish me"

{-
---------------------------------------------------------------------------------------------------
-- | Print the query AST to stdout.
graphWith 
        :: FilePath     -- ^ Root path to data.
        -> Q Query      -- ^ Query.
        -> IO ()

graphWith pathRootData qquery
 = do   let config  = Q.Config pathRootData
        equery <- Q.runQ config qquery
        case equery of
         Left err
          -> error (show err)

         Right q
          -> do putStrLn $ show q
                return ()


---------------------------------------------------------------------------------------------------
-- | Print the JSON version of a query to stdout.
jsonWith :: FilePath     -- ^ Root path to data.
         -> Q Query      -- ^ Query.
         -> IO ()

jsonWith pathRootData qquery
 = do   let config  = Q.Config pathRootData
        equery <- Q.runQ config qquery
        case equery of
         Left err
          -> error (show err)

         Right q
          -> BS.putStrLn
                $ Aeson.encodePretty' aesonConfig
                $ Aeson.toJSON q

aesonConfig
 = Aeson.defConfig
        { Aeson.confIndent      = 4
        , Aeson.confCompare     = compare }


---------------------------------------------------------------------------------------------------
-- | Print the Repa version of a query to stdout.
repaWith :: FilePath    -- ^ Root path to data.
         -> Q Query     -- ^ Query.
         -> IO ()

repaWith pathRootData qquery
 = do   let config = Q.Config pathRootData
        equery <- Q.runQ config qquery
        case equery of
         Left err
          -> error (show err)

         Right q
          -> do dec <- TH.runQ 
                    $ Q.decOfQuery (TH.mkName "_makeSources") q
                putStrLn $ TH.pprint dec

-}

