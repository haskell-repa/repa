
module Data.Array.Repa.Flow.Seq.Source
        ( module Data.Array.Repa.Flow.Base
        , Source        (..)
        , Step1         (..)
        , Step8         (..)
        , SourceState   (..)
        , startSource
        , joinSourceStates
        , getSourceState
        , flow
        , flowGuts)
where
import Data.Array.Repa.Bulk.Elt
import Data.Array.Repa.Flow.Base
import Data.Array.Repa.Flow.Seq.Base
import qualified Data.Array.Repa.Flow.Seq.Report        as R
import qualified Data.Vector.Unboxed.Mutable            as UM
import Prelude                                          hiding (take)
import GHC.Exts


-- | A `Source` is an incremental element producer. 
--   We can pull elements from a source without knowing where they come from.
--
--   Elements can be produced once at a time, or eight at a time as an
--   optional optimisation.
-- 
data Source mode a
        = forall state. Source
        { -- | Representation of the source state depends on whether the
          --   source has already been started.
          sourceState   :: SourceState mode state

          -- | How many elements are still available.
        , sourceSize    :: state -> IO Size

          -- | Report the current state of this flow.
        , sourceReport  :: state -> IO R.Report

          -- | Takes a continuation and calls it with
          --   a `Step1` containing some data.
        , sourceGet1    :: state -> (Step1 a -> IO ()) -> IO ()

          -- | Takes a continuation and calls it with 
          --  a `Step8` containing some data.
        , sourceGet8    :: state -> (Step8 a -> IO ()) -> IO ()
        }


data Step1 a
        -- | An element and a flag saying whether a full 8 elements are
        --   likely to be available next pull.
        --
        ---  We don't want to *force* the consumer to pull the full 8
        --   if it doesn't want to, otherwise functions like folds would
        --   become too complicated.
        = Yield1 a Bool

        -- | The source is finished, no more elements will ever be available.
        | Done


-- | Provide eight elements in one go, or says try to pull the full eight
--   later. The two cases are split like this to force loop unrolling in
--   the intermediate code.
data Step8 a
        -- | Eight successive elements of the flow.
        = Yield8 a a a a a a a a

        -- | The source cannot yield a full 8 elements right now.
        --   You should use `sourceGet1` to get the next element and try
        --  `sourceGet8` again later.
        | Pull1


-------------------------------------------------------------------------------
-- | Holds an action to start the source, 
--   or the current state if it has already been started.
data SourceState mode state where
        SourceStateDelayed 
         :: IO state 
         -> SourceState FD state

        SourceStateActive
         :: state
         -> SourceState FS state


-- | Start a source, making it active.
startSource :: Source FD a -> IO (Source FS a)
startSource (Source istate size report get1 get8)
 = do   state   <- getSourceState istate
        return  $ Source (SourceStateActive state) size report get1 get8


-- | Join two source states of the same mode.
--
--   * If both states are delayed it the resulting action starts both.
--
--   * If both states are already active the result returns both.
--
joinSourceStates 
        :: SourceState mode stateA
        -> SourceState mode stateB
        -> SourceState mode (stateA, stateB)

joinSourceStates 
        (SourceStateDelayed startA)
        (SourceStateDelayed startB)
 = SourceStateDelayed
 $ do   stateA  <- startA
        stateB  <- startB
        return  $ (stateA, stateB)

joinSourceStates
        (SourceStateActive stateA)
        (SourceStateActive stateB)
 = SourceStateActive (stateA, stateB)

joinSourceStates _ _
 = error "joinSourceStates: bogus warning suppression"
{-# INLINE joinSourceStates #-}


-- | Start a source state, 
--   or return the exising state if it has already been started.
getSourceState :: SourceState mode state -> IO state
getSourceState fstate
 = case fstate of
        SourceStateDelayed mkState      -> mkState
        SourceStateActive state         -> return state
{-# INLINE getSourceState #-}


-------------------------------------------------------------------------------
-- | Create a delayed source based on the element index of the flow.
--
--   This is typically used to read elements from some randomly accessible vector.
--
flow    :: Elt a 
        => Int#         -- ^ Total number of elements.
        -> (Int# -> a)  -- ^ Function to get the element at the given index.
        -> Source FD a

flow !len !load 
 | (istate, size, report, get1, get8) <- flowGuts len load
 = Source istate size report get1 get8
{-# INLINE [1] flow #-}


flowGuts
        :: Elt a
        => Int#
        -> (Int# -> a)
        -> ( SourceState FD (UM.IOVector Int)
           , UM.IOVector Int -> IO Size
           , UM.IOVector Int -> IO R.Report
           , UM.IOVector Int -> (Step1 a -> IO ()) -> IO ()
           , UM.IOVector Int -> (Step8 a -> IO ()) -> IO ())

flowGuts !len load
 = (istate, size, report, get1, get8)
 where  
        here    = "seq.flow"

        istate  = SourceStateDelayed
                $ do    refIx   <- inew 1
                        iwrite here refIx 0# 0#
                        return refIx
        {-# INLINE istate #-}


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
{-# INLINE [1] flowGuts #-}

