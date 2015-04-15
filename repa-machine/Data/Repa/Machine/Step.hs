
module Data.Repa.Machine.Step
        ( step
        , ErrorStep (..))
where
import Data.Repa.Machine.Base
import Data.Repa.Query.Eval.Env                 as Env
import Data.Repa.Query.Eval.Exp
import qualified Data.Repa.Machine.Transition   as T
import qualified Data.Map.Strict                as Map


-------------------------------------------------------------------------------
-- | Take a single transition.
step    :: forall label env m
        .  ( Ord label, Ord (Bound env)
           , Monad m)

        => (Bound env -> m (Maybe (Val env)))   
                        -- ^ Get a value from an input stream.

        -> (Bound env -> Maybe (Val env) -> m ())    
                        -- ^ Put a value to   an output stream,
                        --   or signal that this output is done.

        -> Machine label env 
                        -- ^ Starting machine.

        -> m (Either (ErrorStep label (Bound env))
                     (Machine   label env))

step get put mm@(Machine{})
 | lStart       <- machineState mm
 , Just trans   <- Map.lookup (machineState mm) (machineTrans mm)
 = case trans of

        -- Read from a named stream -----------------------
        T.TrPull nInput lSome lNone
         -- Cannot pull a new value while we have one buffered
         -- for the same stream.
         | Just (Just {}) <- Env.lookup nInput (machineElems mm)
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
                  ->    return $ Left $ ErrorStepEvalStuck lStart

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

        _ -> error "step: finish transitions"

 | otherwise
 = return $ Left $ ErrorStepInvalidState (machineState mm)


-- | Things that can go wrong when stepping a machine.
data ErrorStep lState nStream
        -- | Tried to pull from an input stream when the associated
        --   buffer already contained an element.
        = ErrorStepPullFull      lState nStream

        -- | Tried to release an input value when we weren't holding it.
        | ErrorStepReleaseEmpty  lState nStream

        -- | Evaluation of scalar expression got stuck.
        | ErrorStepEvalStuck     lState

        -- | Machine ended up in some unknown state.
        | ErrorStepInvalidState  lState
        deriving Show



