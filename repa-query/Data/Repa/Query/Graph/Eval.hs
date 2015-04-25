
module Data.Repa.Query.Graph.Eval
        ( -- * Environments
          Env (..)
        , ExpEnv
        , ValEnv

          -- * Evaluators
        , evalExp
        , evalScalarOp)
where
import Data.Repa.Query.Graph.Eval.Env
import Data.Repa.Query.Graph.Eval.Exp
