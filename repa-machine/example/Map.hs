

import Data.Repa.Merge.Machine
import Data.Repa.Merge.Transition
import Control.Monad.ST
import Data.STRef
import Data.Map                                 (Map)
import Data.Repa.Query.Eval.Env                 (Env)
import qualified Data.Repa.Query.Eval.Env       as Env
import qualified Data.Repa.Query.Exp            as Exp
import qualified Data.Map                       as Map

import Debug.Trace

mmap    :: Machine String (Map String)
mmap
 = Machine "pull" 
        False 
        (Map.fromList tt)
        (Env.fromList [("x", Nothing)])
        (Env.fromList [("y", Nothing)])
 where 
  tt =  [ ("pull",     TrPull           "x" "out"   "eject")
        , ("out",       TrOut           "y" xx      "release")
        , ("release",   TrRelease       "x" "pull")
        , ("eject",     TrEject         "y" "halt")
        , ("halt",      TrHalt) ]

  xx = Exp.XOp () Exp.SopMul [Exp.XVar () "x", Exp.xInt () 2]


main 
 = putStrLn "foo"


lInts :: [Integer] -> [Lit]
lInts is = map Exp.LInt is
