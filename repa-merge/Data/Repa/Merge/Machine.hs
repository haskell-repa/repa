
module Data.Repa.Merge.Machine
        (Machine (..))
where
import Data.Repa.Merge.Transition
import Data.Repa.Query.Graph
import qualified Data.Map.Strict                as Map
import Data.Map                                 (Map)


-- | A state machine that reads data from some input streams and
--   writes to some output streams.
data Machine lState nStream aExp bVal uVal
        = Machine
        { -- | State the machine is currently in.
          machineState  :: lState

          -- | Whether the machine has already finished and halted.
        , machineHalt   :: Bool

          -- | Machine transitions.
        , machineTrans  :: Map lState  (Transition lState nStream aExp bVal uVal) 

          -- | Environment attached to each stream.
        , machineEnv    :: Map nStream (Exp aExp bVal uVal)

          -- | Current element buffered from each stream.
        , machineElems  :: Map nStream (Maybe (Val aExp bVal uVal))
        }
        deriving Show


-- | Take a single transition.
step    :: (Ord lState, Ord nStream, Monad m)
        => (nStream -> m (Maybe (Val aExp bVal uVal)))
                        -- ^ Get a value from an input stream.
        -> (nStream -> Maybe (Val aExp bVal uVal) -> m ())
                        -- ^ Put a value to   an output stream,
                        --   or signal that this output is done.
        -> Machine lState nStream aExp bVal uVal       
                        -- ^ Starting machine.
        -> m (Either (ErrorStep lState nStream) 
                     (Machine lState nStream aExp bVal uVal))

step get put mm
 | lStart       <- machineState mm
 , Just trans   <- Map.lookup (machineState mm) (machineTrans mm)
 = case trans of

        -- Read from a named stream -----------------------
        TrPull nInput lSome lNone
         -- Cannot pull a new value while we have one buffered
         -- for the same stream.
         | Just{}       <- Map.lookup nInput (machineElems mm)
         -> return $ Left $ ErrorStepPullFull lStart nInput

         -- Pull a new value from an input.
         | otherwise
         -> do  mVal    <- get nInput
                case mVal of
                 Just val
                  -> return $ Right
                  $  mm  { machineState = lSome 
                         , machineElems = Map.insert nInput (Just val) (machineElems mm) }

                 Nothing
                  -> return $ Right
                  $  mm  { machineState = lNone }


        -- Release currently buffered value ---------------
        TrRelease nInput lNext
         -- Cannot release when there is no buffered element from
         -- the named stream.
         | Nothing      <- Map.lookup nInput (machineElems mm)
         -> return $ Left $ ErrorStepReleaseEmpty lStart nInput

         | otherwise
         -> return $ Right
                $ mm { machineElems = Map.insert nInput Nothing (machineElems mm) }


        -- Skip to the next state -------------------------
        TrSkip lNext    
         -> return $ Right $ mm { machineState = lNext }




data ErrorStep lState nStream
        = ErrorStepPullFull     lState nStream
        | ErrorStepReleaseEmpty  lState nStream

