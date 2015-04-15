

module Data.Repa.Machine.Base
        ( Machine       (..)
        , Transition
        , Exp, Val, G.Lit)
where
import Data.Repa.Query.Eval.Env                 as Env
import qualified Data.Repa.Machine.Transition   as T
import qualified Data.Repa.Query.Graph          as G
import Data.Map                                 (Map)


-------------------------------------------------------------------------------
-- | An expression closed by some environment.
type Exp env
        = G.Exp () (Bind env) (Bound env)


-- | A value closed by some environment.
type Val env
        = G.Val () (Bind env) (Bound env)



-- | A transition with between states with some label, 
--   where the expressions are closed with some environment.
type Transition label env
        = T.Transition label (Bound env) () (Bind env) (Bound env)


-------------------------------------------------------------------------------
-- | A state machine that reads data from some input streams and
--   writes to some output streams.
data Machine label env
        =  ( Env env (Val env)
           , Env env (Maybe (Val env)))
        => Machine
        { -- | State the machine is currently in.
          machineState  :: label

          -- | Whether the machine has already finished and halted.
        , machineHalt   :: Bool

          -- | Machine transitions.
        , machineTrans  :: Map label (Transition label env) 

          -- | Input value buffered from each input stream.
        , machineElems  :: env (Maybe (Val env)) 

          -- | State value attached to each output stream.
        , machineEnv    :: env (Maybe (Val env)) }

