
module Data.Array.Repa.Flow.Seq.Operator.Dup
        (dup_cc)
where
import Data.Array.Repa.Flow.Seq.CoFlow


-- | Create a coflow that pushes elements into two others.
dup_cc :: CoFlow mode a -> CoFlow mode a -> CoFlow mode a
dup_cc    (CoFlow cfstateA ejectA feed1A feed8A)
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

