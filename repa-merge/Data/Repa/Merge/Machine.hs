
module Data.Repa.Merge.Machine
        (Machine (..))
where
import Data.Repa.Query.Eval.Env                 as Env
import Data.Repa.Query.Eval.Exp
import qualified Data.Repa.Merge.Transition     as T
import qualified Data.Repa.Query.Graph          as G
import qualified Data.Map.Strict                as Map
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
        = Machine
        { -- | State the machine is currently in.
          machineState  :: label

          -- | Whether the machine has already finished and halted.
        , machineHalt   :: Bool

          -- | Machine transitions.
        , machineTrans  :: Map label (Transition label env) 

          -- | State value attached to each output stream.
        , machineEnv    :: env (Exp env)

          -- | Input value buffered from each input stream.
        , machineElems  :: env (Maybe (Val env)) }


-------------------------------------------------------------------------------
-- | Take a single transition.
step    :: forall label env m
        .  ( Ord label, Ord (Bound env)
           , Monad m
           , Env env (Val env)
           , Env env (Maybe (Val env)))

        => (Bound env -> m (Maybe (Val env)))   
                        -- ^ Get a value from an input stream.

        -> (Bound env -> Maybe (Val env) -> m ())    
                        -- ^ Put a value to   an output stream,
                        --   or signal that this output is done.

        -> Machine label env 
                        -- ^ Starting machine.

        -> m (Either (ErrorStep label (Bound env))
                     (Machine   label env))

step get put mm
 | lStart       <- machineState mm
 , Just trans   <- Map.lookup (machineState mm) (machineTrans mm)
 = case trans of

        -- Read from a named stream -----------------------
        T.TrPull nInput lSome lNone
         -- Cannot pull a new value while we have one buffered
         -- for the same stream.
         | Just{}       <- Env.lookup nInput (machineElems mm)
         -> return $ Left $ ErrorStepPullFull lStart nInput

         -- Pull a new value from an input.
         | otherwise
         -> do  mVal    <- get nInput
                case mVal of
                 Just val
                  -> return $ Right
                  $  mm  { machineState = lSome 
                         , machineElems = Env.update nInput (Just val) (machineElems mm) }

                 Nothing
                  -> return $ Right
                  $  mm  { machineState = lNone }


        -- Release currently buffered value ---------------
        T.TrRelease nInput lNext
         -- Cannot release when there is no buffered element from
         -- the named stream.
         | Just Nothing <- Env.lookup nInput (machineElems mm)
         -> return $ Left $ ErrorStepReleaseEmpty lStart nInput

         | otherwise
         -> return $ Right
         $  mm { machineState = lNext
               , machineElems = Env.update nInput Nothing (machineElems mm) }


        -- Output a value to a stream ---------------------
        T.TrOut nOutput xFun lNext
         -> do  let nvsInput    = [(n, v) | (n, Just v) <- Env.toList (machineElems mm)]
                let env :: env (Val env) = Env.fromList nvsInput
                case evalExp env xFun of
                 Nothing        
                  ->    return $ Left $ ErrorEvalStuck lStart

                 Just vResult   
                  -> do put nOutput (Just vResult)
                        return $ Right mm { machineState = lNext }


        -- Eject an output stream -------------------------
        T.TrEject nOutput lNext
         -> do  put nOutput Nothing
                return  $ Right mm { machineState = lNext }


        -- Skip to the next state -------------------------
        T.TrSkip lNext    
         -> return $ Right $ mm { machineState = lNext }


        -- Halt the machine -------------------------------
        T.TrHalt 
         -> return $ Right $ mm { machineHalt  = True  }


data ErrorStep lState nStream
        = ErrorStepPullFull      lState nStream
        | ErrorStepReleaseEmpty  lState nStream
        | ErrorEvalStuck         lState


