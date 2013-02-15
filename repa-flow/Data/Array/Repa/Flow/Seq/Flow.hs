

-- | Flows provide an incremental version of array fusion that allows the
--   the computation to be suspended and resumed at a later time.
module Data.Array.Repa.Flow.Seq.Flow
        ( module Data.Array.Repa.Flow.Base
        , Flow          (..)
        , Step1         (..)
        , Step8         (..)
        , FlowState     (..)
        , startFlow
        , joinFlowStates
        , getFlowState
        , flow)
where
import Data.Array.Repa.Bulk.Elt
import Data.Array.Repa.Flow.Base
import Data.Array.Repa.Flow.Seq.Base
import qualified Data.Array.Repa.Flow.Seq.Report        as R
import Prelude                                          hiding (take)
import GHC.Exts


-- | A `Flow` is an incremental element producer. We can pull elements from 
--   a flow without knowing where they come from.
--
--   Elements can be produced once at a time, or eight at a time as an
--   optional optimisation.
-- 
data Flow mode a
        = forall state. Flow
        { 
          -- | Representation of the flow state depends on whether the flow
          --   has already been started.
          flowState     :: FlowState mode state

          -- | How many elements are still available.
        , flowSize      :: state -> IO Size

          -- | Report the current state of this flow.
        , flowReport    :: state -> IO R.Report

          -- | Takes a continuation and calls it with
          --   a `Step1` containing some data.
        , flowGet1      :: state -> (Step1 a -> IO ()) -> IO ()

          -- | Takes a continuation and calls it with 
          --  a `Step8` containing some data.
        , flowGet8      :: state -> (Step8 a -> IO ()) -> IO ()
        }


data Step1 a
        -- | An element and a flag saying whether a full 8 elements are
        --   likely to be available next pull.
        --
        ---  We don't want to *force* the consumer to pull the full 8
        --   if it doesn't want to, otherwise functions like folds would
        --   become too complicated.
        = Yield1 a Bool

        -- | The flow is finished, no more elements will ever be available.
        | Done


-- | Provide eight elements in one go, or says try to pull the full eight
--   later. The two cases are split like this to force loop unrolling in
--   the intermediate code.
data Step8 a
        -- | Eight successive elements of the flow.
        = Yield8 a a a a a a a a

        -- | The flow cannot yield a full 8 elements right now.
        --   You should use `flowGet1` to get the next element and try
        --  `flowGet8` again later.
        | Pull1


-- | Holds an action to start the flow, or the current state if it has
--   already been started.
data FlowState mode state where
        FlowStateDelayed 
         :: IO state 
         -> FlowState FD state

        FlowStateActive
         :: state
         -> FlowState FS state


-- | Start a flow, making it active.
startFlow :: Flow FD a -> IO (Flow FS a)
startFlow (Flow fstate size report get1 get8)
 = do   state   <- getFlowState fstate
        return  $ Flow (FlowStateActive state) size report get1 get8


-- | Join two flow states of the same mode.
--
--   If both flow states are delayed it the resulting action starts both.
--
--   If both flow states are already active the result returns both.
--
joinFlowStates 
        :: FlowState mode stateA
        -> FlowState mode stateB
        -> FlowState mode (stateA, stateB)

joinFlowStates 
        (FlowStateDelayed startA)
        (FlowStateDelayed startB)
 = FlowStateDelayed
 $ do   stateA  <- startA
        stateB  <- startB
        return  $ (stateA, stateB)

joinFlowStates
        (FlowStateActive stateA)
        (FlowStateActive stateB)
 = FlowStateActive (stateA, stateB)

joinFlowStates _ _
 = error "joinFlowStates: bogus warning suppression"
{-# INLINE joinFlowStates #-}


-- | Start a flow state, 
--   or return the exising state if it has already been started.
getFlowState :: FlowState mode state -> IO state
getFlowState fstate
 = case fstate of
        FlowStateDelayed makeFlowState
         -> makeFlowState

        FlowStateActive state
         -> return state
{-# INLINE getFlowState #-}


-------------------------------------------------------------------------------
-- | Create a delayed flow that will read elements from some randomly
--   accessible vector.
flow    :: Elt a 
        => Int#         -- ^ Total number of elements.
        -> (Int# -> a)  -- ^ Function to get the element at the given index.
        -> Flow FD a

flow !len !load 
 = Flow state size report get1 get8
 where  
        here    = "repa-flow.seq.flow"

        state   = FlowStateDelayed
                $ do    refIx   <- inew 1
                        iwrite here refIx 0# 0#
                        return refIx
        {-# INLINE state #-}

        size refIx

         = do   !(I# ix)        <- iread here refIx 0#
                return  $ Exact (len -# ix)
        {-# INLINE size #-}


        report refIx
         = do   !ix             <- iread here refIx 0#
                return  $ R.Flow (I# len) ix
        {-# NOINLINE report #-}


        get1 refIx push1
         = do   !(I# ix)        <- iread here refIx 0#
                let !remain     =  len -# ix
                if remain ># 0#
                 then do
                        iwrite here refIx 0# (ix +# 1#)
                        let !x  = load ix

                        -- Touch because we want to be sure its unboxed as
                        -- soon as we read it. It we don't touch it, and
                        -- the continuation uses the value in multiple
                        -- case branches then it can be reboxed and then
                        -- unboxed again multiple times.
                        touch x

                        push1 $ Yield1 x (remain >=# 9#)

                 else   push1 Done
        {-# INLINE get1 #-}


        get8 refIx push8
         = do   !(I# ix)        <- iread here refIx 0#
                let !remain     = len -# ix
                if remain >=# 8#
                 then do
                        iwrite here refIx 0# (ix +# 8#)

                        -- TODO: not sure whether we should force these here
                        let here' = return

                        !x0     <- here' $ load (ix +# 0#)
                        !x1     <- here' $ load (ix +# 1#)
                        !x2     <- here' $ load (ix +# 2#)
                        !x3     <- here' $ load (ix +# 3#)
                        !x4     <- here' $ load (ix +# 4#)
                        !x5     <- here' $ load (ix +# 5#)
                        !x6     <- here' $ load (ix +# 6#)
                        !x7     <- here' $ load (ix +# 7#)

                        push8 $ Yield8 x0 x1 x2 x3 x4 x5 x6 x7

                 else do
                        push8 Pull1
        {-# INLINE get8 #-}

{-# INLINE [1] flow #-}

