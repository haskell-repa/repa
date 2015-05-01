
module Data.Repa.Query.Job
        ( -- * Jobs
          Job           (..)

          -- * Query
        , Query         (..)
        , QueryS        
        , OutputFormat  (..)

          -- * Extract
        , ExtractTarget (..)

          -- * Job Execution
        , runWith
        , RunError      (..)

          -- * Job Explanations
        , explainWith
        , ExplainFormat (..)
        , ExplainError  (..))
where
import Data.Repa.Query.Job.Exec
import Data.Repa.Query.Job.Spec 
