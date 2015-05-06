
module Data.Repa.Query.Job.Exec
        ( -- * Running Jobs
          runWith
        , RunError (..)

          -- * Explaining Jobs
        , explainWith
        , ExplainFormat (..)
        , ExplainError  (..))
where
import qualified BuildBox                               as BB
import qualified BuildBox.Command.File                  as BB

import Data.Repa.Query.Job.Spec
import qualified Data.Repa.Query.Source.Builder         as Q
import qualified Data.Repa.Query.Build                  as Q
import qualified Data.Repa.Query.Job.JSON               ()

import qualified System.Process                         as S

import qualified Data.Aeson                             as Aeson
import qualified Data.Aeson.Encode.Pretty               as Aeson

import qualified Data.ByteString.Lazy.Char8             as BS


---------------------------------------------------------------------------------------------------
-- | Run a sequence of jobs.
runWith :: FilePath             -- ^ Local scratch dir for temp files.
        -> FilePath             -- ^ Root path to data.
        -> (Q.Config -> IO (Either String [Job])) 
                                -- ^ Job builder.
        -> IO (Maybe RunError)

runWith pathScratch pathRootData mkJobs
 = do   
        -- Execute the job builder to get a list of jobs to run.
        -- The builder might fail with some error if the job description
        -- is malformed, or it can't find appropriate meta-data.
        let config  = Q.Config pathRootData
        eJobs      <- mkJobs config

        case eJobs of
         Left err   
          -> return $ Just $ RunErrorSpec err

         Right jobs 
          -> do result     <- BB.runBuild pathScratch (runJobs jobs)
                case result of
                 Left err -> return $ Just $ RunErrorExec err
                 Right () -> return Nothing

 where
        runJobs []      = return ()
        runJobs (j:js)  = runJob j >> runJobs js

        runJob job
         = BB.withTempFile $ \pathExe
         -> do  Q.buildJobViaRepa 
                        pathScratch True
                        job pathExe

                _ <- BB.io 
                        $ S.system 
                        $ "cp " ++ pathExe ++ " /tmp/Main.bin"

                BB.io $ S.system 
                      $ pathExe ++ " -root-data " ++ pathRootData




data RunError
        -- | Error when creating the job specification.
        = RunErrorSpec String

        -- | Error when executing a job.
        | RunErrorExec BB.BuildError
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Explain a sequence of jobs,
--   printing the descriptions to stdout.
explainWith
        :: FilePath             -- ^ Local scratch dir for temp files.
        -> FilePath             -- ^ Root path to data.
        -> ExplainFormat        -- ^ How to format the job descriptions.
        -> (Q.Config -> IO (Either String [Job]))
                                -- ^ Job builder.
        -> IO (Maybe ExplainError)

explainWith _pathScratch pathRootData explainFormat mkJobs
 = do   
        let config  = Q.Config pathRootData
        eJobs       <- mkJobs config

        case eJobs of
         Left err
          -> return $ Just $ ExplainErrorSpec err

         Right jobs
          -> case explainFormat of
                Graph   
                 -> do  putStrLn $ show jobs 
                        return Nothing

                Json
                 -> do  BS.putStrLn
                         $ Aeson.encodePretty' aesonConfig
                         $ Aeson.toJSON jobs

                        return Nothing


-- | How to explain some jobs.
data ExplainFormat
        = Graph         -- ^ Show the Job AST in internal Haskell format.
        | Json          -- ^ Show the Job AST in JSON format.
        deriving Show


-- | Things that can go wrong when explaining jobs.
data ExplainError
        -- | Error when creating the job specification.
        = ExplainErrorSpec String
        deriving Show


aesonConfig
 = Aeson.defConfig
        { Aeson.confIndent      = 4
        , Aeson.confCompare     = compare }

