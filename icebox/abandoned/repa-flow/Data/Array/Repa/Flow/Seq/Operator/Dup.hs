
-- | Constant space flow duplication.
--
--   We cannot supply a @dup_ii@ function. To handle the case where the
--   entire flow was pulled from one out-flow before the other, we would need
--   to buffer the entire flow.
--
module Data.Array.Repa.Flow.Seq.Operator.Dup
        ( dup_oo
        , dup_io
        , dup_oi)
where
import Data.Array.Repa.Flow.Seq.Source
import Data.Array.Repa.Flow.Seq.Sink


-- | Create a coflow that pushes elements into two others.
dup_oo :: Sink mode a -> Sink mode a -> Sink mode a
dup_oo  (Sink ostateA ejectA feed1A feed8A)
        (Sink ostateB ejectB feed1B feed8B)
 =       Sink ostate' eject' feed1' feed8'
 where  
        ostate'
         = joinSinkStates ostateA ostateB

        eject' (stateA, stateB)
         = do   ejectA stateA
                ejectB stateB

        feed1'  (stateA, stateB) snack1
         = do   feed1A stateA snack1
                feed1B stateB snack1

        feed8'  (stateA, stateB) snack8
         = do   feed8A stateA snack8
                feed8B stateB snack8
{-# INLINE [1] dup_oo #-}


-- | Duplicate a flow by pushing all elements pulled from a source
--   to a separate sink.
dup_io :: Source mode a -> Sink mode a -> Source mode a
dup_io  (Source mkStateA  getSizeA  reportA get1  _get8)
        (Sink   mkStateB  ejectB            feed1 _feed8)
 =       Source mkState'  getSize'  report' get1' get8'
 where  
        mkState'
         | SourceStateDelayed startA      <- mkStateA
         , SinkStateDelayed   startB      <- mkStateB
         = SourceStateDelayed
         $ do   stateA  <- startA
                sizeA   <- getSizeA stateA
                stateB  <- startB   sizeA
                return  (stateA, stateB)

         | SourceStateActive  stateA      <- mkStateA
         , SinkStateActive    stateB      <- mkStateB
         = SourceStateActive
                (stateA, stateB)

         | otherwise
         = error "seq.dup_io: bogus warning suppression"

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
dup_oi :: Sink mode a -> Source mode a -> Source mode a
dup_oi oo ii
        = dup_io ii oo
{-# INLINE [1] dup_oi #-}

