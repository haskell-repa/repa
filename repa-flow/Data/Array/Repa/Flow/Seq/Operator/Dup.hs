
module Data.Array.Repa.Flow.Seq.Operator.Dup
        ( dup_oo
        , dup_io
        , dup_oi)
where
import Data.Array.Repa.Flow.Seq.Flow
import Data.Array.Repa.Flow.Seq.Sink


-- | Create a coflow that pushes elements into two others.
dup_oo :: Sink mode a -> Sink mode a -> Sink mode a
dup_oo  (Sink ostateA ejectA feed1A feed8A)
        (Sink ostateB ejectB feed1B feed8B)
 =       Sink ostateZ ejectZ feed1Z feed8Z
 where  
        ostateZ
         = joinSinkStates ostateA ostateB

        ejectZ (stateA, stateB)
         = do   ejectA stateA
                ejectB stateB

        feed1Z  (stateA, stateB) snack1
         = do   feed1A stateA snack1
                feed1B stateB snack1

        feed8Z  (stateA, stateB) snack8
         = do   feed8A stateA snack8
                feed8B stateB snack8
{-# INLINE [1] dup_oo #-}


-- | Create a flow that pushes elements into another coflow before
--   returning them.
dup_io :: Flow mode a -> Sink mode a -> Flow mode a
dup_io  (Flow  mkStateA  getSizeA  reportA get1  _get8)
        (Sink  mkStateB  ejectB            feed1 _feed8)
 =       Flow  mkState'  getSize'  report' get1' get8'
 where  
        mkState'
         | FlowStateDelayed startA      <- mkStateA
         , SinkStateDelayed startB      <- mkStateB
         = FlowStateDelayed
         $ do   stateA  <- startA
                sizeA   <- getSizeA stateA
                stateB  <- startB   sizeA
                return  (stateA, stateB)

         | FlowStateActive   stateA     <- mkStateA
         , SinkStateActive stateB       <- mkStateB
         = FlowStateActive
                (stateA, stateB)

         | otherwise
         = error "repa-flow.seq.dup_io: bogus warning suppression"

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

{-# INLINE [1] dup_io #-}


-- | As above, but with the parameters flipped.
dup_oi :: Sink mode a -> Flow mode a -> Flow mode a
dup_oi oo ii
        = dup_io ii oo
{-# INLINE [1] dup_oi #-}

