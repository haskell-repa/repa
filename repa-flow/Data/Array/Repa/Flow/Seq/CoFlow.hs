
module Data.Array.Repa.Flow.Seq.CoFlow
        ( CoFlow        (..)
        , Snack1        (..)
        , Snack8        (..)
        , CoFlowState   (..)
        , startCoFlow
        , joinCoFlowStates
        , getCoFlowState)
where
import Data.Array.Repa.Flow.Seq.Base


-- | A 'CoFlow' is an abstract element consumer. We can push elements into
--   a coflow without knowing where they go.
--
--   Once the coflow is started, we feed elements into it and then call
--   the eject function to indicate end-of-input.
data CoFlow mode a
        = forall state. CoFlow
        { -- | The representation of the coflow state depends on its mode.
          coflowState   :: CoFlowState mode state

          -- | Signal that we've fed the coflow all available elements.
          --   This is done separately as from the feed functions so that they 
          --   don't need to check for end-of-input conditions on every iteration.
          --
          --  * Calling this more than once on a given coflow is undefined.
          --
          --  *  Calling the feed functions after doing this is undefined.
        , coflowEject   :: state -> IO ()

          -- | Feed a single element to the coflow.
        , coflowFeed1   :: state -> Snack1 a -> IO ()

          -- | Feed eight elements to the coflow.
        , coflowFeed8   :: state -> Snack8 a -> IO () }


-- | Wraps an element to feed to a CoFlow.
data Snack1 a
        = Snack1 a

-- | Wraps eight elements to feed to a CoFlow.
data Snack8 a
        = Snack8 a a a a a a a a


data CoFlowState mode state where
        CoFlowStateDelayed
          --   Start the coflow, given the maximum number of elements
          --   we intend to feed to it. This returns a state value that
          --   needs to be passed to the other functions.
          --
          --   Calling this more than once on a given coflow is undefined.
          --
          --   Calling the other functions before doing this is undefined.
         :: (Size -> IO state)

         -> CoFlowState FD state

        CoFlowStateActive 
          -- The current state of the coflow.
         :: state
         -> CoFlowState FS state


-- | Start the coflow, making it active.
startCoFlow :: Size -> CoFlow FD a -> IO (CoFlow FS a)
startCoFlow size (CoFlow cfstate eject feed1 feed8)
 = do   state   <- getCoFlowState size cfstate 
        return  $  CoFlow (CoFlowStateActive state) eject feed1 feed8


-- | Join two co-flow states in the same mode.
joinCoFlowStates 
        :: CoFlowState mode stateA
        -> CoFlowState mode stateB
        -> CoFlowState mode (stateA, stateB)

joinCoFlowStates 
        (CoFlowStateDelayed startA) 
        (CoFlowStateDelayed startB)
 = CoFlowStateDelayed $ \size
 -> do  sA      <- startA size
        sB      <- startB size
        return (sA, sB)

joinCoFlowStates
        (CoFlowStateActive stateA)
        (CoFlowStateActive stateB)
 = CoFlowStateActive
        (stateA,   stateB)

joinCoFlowStates _ _
 = error "appCoFlowStates: bogus warning suppression"


-- | Start a co-flow, or return the existing state if it has already
--   been started.
getCoFlowState :: Size -> CoFlowState mode state -> IO state
getCoFlowState size cfstate
 = case cfstate of
        CoFlowStateDelayed getState
         -> getState size

        CoFlowStateActive state
         -> return state




