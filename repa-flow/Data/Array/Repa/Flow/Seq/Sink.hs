
module Data.Array.Repa.Flow.Seq.Sink
        ( Sink          (..)
        , Snack1        (..)
        , Snack8        (..)
        , SinkState     (..)
        , startSink
        , joinSinkStates
        , getSinkState
        , unflowIO)
where
import Data.Array.Repa.Flow.Seq.Base
import Data.Array.Repa.Flow.Base
import Data.IORef
import GHC.Exts


-- | A 'Sink' is an abstract element consumer. We can push elements into
--   a sink without knowing where they go.
--
--   Once the sink is started, we feed elements into it and then call
--   the eject function to indicate end-of-input.
data Sink mode a
        = forall state. Sink
        { -- | The representation of the sink state depends on its mode.
          sinkState     :: SinkState mode state

          -- | Signal that we've fed the sink all available elements.
          --   This is done separately from the feed functions so that the
          --   generated code doesn't need to check for end-of-input condition
          --   on every iteration. A sink can be safely ejected multiple times.
        , sinkEject     :: state -> IO ()

          -- | Feed a single element to the sink.
        , sinkFeed1     :: state -> Snack1 a -> IO ()

          -- | Feed eight elements to the sink.
        , sinkFeed8     :: state -> Snack8 a -> IO () }


-- | Wraps an element to feed to a Sink.
data Snack1 a
        = Snack1 a

-- | Wraps eight elements to feed to a Sink.
data Snack8 a
        = Snack8 a a a a a a a a


-------------------------------------------------------------------------------
data SinkState mode state where
        SinkStateDelayed
          --  Start the sink, given the maximum number of elements we intend
          --  to feed to it. This returns a state value that needs to be
          --  passed to the other functions. Calling this on an sink that has
          --  already been started returns the current state.
         :: (Size -> IO state)
         -> SinkState FD state

        SinkStateActive 
          -- The current state of the sink.
         :: state
         -> SinkState FS state


-- | Start the sink, making it active.
startSink :: Size -> Sink FD a -> IO (Sink FS a)
startSink size (Sink ostate eject feed1 feed8)
 = do   state   <- getSinkState size ostate
        return  $  Sink (SinkStateActive state) eject feed1 feed8


-- | Join two sink states of the same mode.
joinSinkStates 
        :: SinkState mode stateA
        -> SinkState mode stateB
        -> SinkState mode (stateA, stateB)

joinSinkStates 
        (SinkStateDelayed startA) 
        (SinkStateDelayed startB)
 = SinkStateDelayed $ \size
 -> do  sA      <- startA size
        sB      <- startB size
        return (sA, sB)

joinSinkStates
        (SinkStateActive stateA)
        (SinkStateActive stateB)
 = SinkStateActive
        (stateA,   stateB)

joinSinkStates _ _
 = error "joinSinkStates: bogus warning suppression"


-- | Start a co-flow, or return the existing state if it has already
--   been started.
getSinkState :: Size -> SinkState mode state -> IO state
getSinkState size ostate
 = case ostate of
        SinkStateDelayed getState -> getState size
        SinkStateActive state     -> return state


-------------------------------------------------------------------------------
-- | Create a delayed sink that will write elements into a randomly
--   accessable mutable vector.
unflowIO 
        :: (Int#  -> IO (vec a))         -- ^ Create a new vector.
        -> (vec a -> Int# -> a -> IO ()) -- ^ Write an element into the vector.
        -> (Sink FD a -> IO ())          -- ^ Load elements into the sink.
        -> IO (Int, vec a)               -- ^ Completed vector, 
                                         --    and number of elements written.
unflowIO new write load
 = do   
        let here        = "repa-flow.seq.unflowIO"
        let sIndex      = 0#

        refOut          <- newIORef Nothing

        let ostate
             =  SinkStateDelayed $ \(Max maxLen)
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
        load   $ Sink ostate eject feed1 feed8

        -- Read out the completed values.
        Just (state, vec) <- readIORef refOut
        len               <- iread here state sIndex
        return (len, vec)

