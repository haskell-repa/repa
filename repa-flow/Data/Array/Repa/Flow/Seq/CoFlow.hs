
module Data.Array.Repa.Flow.Seq.CoFlow
        ( CoFlow        (..)
        , Snack1        (..)
        , Snack8        (..)
        , CoFlowState   (..)
        , startCoFlow
        , joinCoFlowStates
        , getCoFlowState
        , unflowIO)
where
import Data.Array.Repa.Flow.Seq.Base
import Data.Array.Repa.Flow.Base
import Data.IORef
import GHC.Exts

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
          --   This is done separately from the feed functions so that the
          --   generated code doesn't need to check for end-of-input condition
          --   on every iteration. A coflow can be safely ejected multiple times.
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


-------------------------------------------------------------------------------
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


-------------------------------------------------------------------------------
-- | Create a delayed coflow that will write elements into a randomly
--   accessable mutable vector.
unflowIO 
        :: (Int#  -> IO (vec a))         -- ^ Action to create a new vector.
        -> (vec a -> Int# -> a -> IO ()) -- ^ Action to write an element into the vector.
        -> (CoFlow FD a -> IO ())        -- ^ Action to load elements into the coflow.
        -> IO (Int, vec a)               -- ^ Completed vector, and number of elements written.

unflowIO new write load
 = do   
        let here        = "repa-flow.seq.unflowIO"
        let sIndex      = 0#

        refOut          <- newIORef Nothing

        let cfstate
             =  CoFlowStateDelayed $ \(Max maxLen)
             -> do state  <- inew 1
                   iwrite here state sIndex 0#

                   vecOut <- new maxLen
                   return (state, vecOut)

        let eject state
             =    writeIORef refOut (Just state)

        let feed1 (state, vecOut) (Snack1 x)
             = do (I# ix) <- iread here state sIndex
                  write vecOut ix x
                  iwrite here state sIndex (ix +# 1#)

        let feed8 (state, vecOut) (Snack8 x0 x1 x2 x3 x4 x5 x6 x7)
             = do (I# ix) <- iread here state sIndex
                  write vecOut (ix +# 0#) x0
                  write vecOut (ix +# 1#) x1
                  write vecOut (ix +# 2#) x2
                  write vecOut (ix +# 3#) x3
                  write vecOut (ix +# 4#) x4
                  write vecOut (ix +# 5#) x5
                  write vecOut (ix +# 6#) x6
                  write vecOut (ix +# 7#) x7
                  iwrite here state sIndex (ix +# 8#)

        -- Use the provided function to load data into the coflow.
        load   $ CoFlow cfstate eject feed1 feed8

        -- Read out the completed values.
        Just (state, vec) <- readIORef refOut
        len               <- iread here state sIndex
        return (len, vec)

