
module Data.Array.Repa.Flow.Seq.Operator.Dup
        ( dup_cc
        , dup_fc
        , dup_cf)
where
import Data.Array.Repa.Flow.Seq.Flow
import Data.Array.Repa.Flow.Seq.CoFlow


-- | Create a coflow that pushes elements into two others.
dup_cc :: CoFlow mode a -> CoFlow mode a -> CoFlow mode a
dup_cc  (CoFlow cfstateA ejectA feed1A feed8A)
        (CoFlow cfstateB ejectB feed1B feed8B)
 =       CoFlow cfstateZ ejectZ feed1Z feed8Z
 where  
        cfstateZ
         = joinCoFlowStates cfstateA cfstateB

        ejectZ (stateA, stateB)
         = do   ejectA stateA
                ejectB stateB

        feed1Z  (stateA, stateB) snack1
         = do   feed1A stateA snack1
                feed1B stateB snack1

        feed8Z  (stateA, stateB) snack8
         = do   feed8A stateA snack8
                feed8B stateB snack8
{-# INLINE [1] dup_cc #-}


-- | Create a flow that pushes elements into another coflow before
--   returning them.
dup_fc :: Flow mode a -> CoFlow mode a -> Flow mode a
dup_fc  (Flow   mkStateA  getSizeA  reportA get1  _get8)
        (CoFlow mkStateB  ejectB            feed1 _feed8)
 =       Flow   mkState'  getSize'  report' get1' get8'
 where  
        mkState'
         | FlowStateDelayed   startA    <- mkStateA
         , CoFlowStateDelayed startB    <- mkStateB
         = FlowStateDelayed
         $ do   stateA  <- startA
                sizeA   <- getSizeA stateA
                stateB  <- startB   sizeA
                return  (stateA, stateB)

         | FlowStateActive   stateA     <- mkStateA
         , CoFlowStateActive stateB     <- mkStateB
         = FlowStateActive
                (stateA, stateB)

         | otherwise
         = error "repa-flow.seq.dup_fc: bogus warning suppression"

        getSize' (stateA, _)
         = getSizeA stateA

        report'  (stateA, _)
         = reportA stateA

        get1' (stateA, stateB) push1
         =  get1 stateA $ \r
         -> case r of
                Yield1 x1 hint
                 -> do  push1 $ Yield1 x1 hint
                        feed1 stateB $ Snack1 x1

                Done
                 -> do  push1 $ Done
                        ejectB stateB

        get8' _ push8
         = push8 $ Pull1

{-# INLINE [1] dup_fc #-}


-- | As above, but with the parameters flipped.
dup_cf :: CoFlow mode a -> Flow mode a -> Flow mode a
dup_cf coflow flow
        = dup_fc flow coflow
{-# INLINE [1] dup_cf #-}

