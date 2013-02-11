
module Data.Array.Repa.Flow.Seq.Operator.Dup
        (dup2)
where
import Data.Array.Repa.Flow.Seq.CoFlow


dup2 :: CoFlow a -> CoFlow a -> CoFlow a
dup2    (CoFlow startA ejectA feed1A feed8A)
        (CoFlow startB ejectB feed1B feed8B)
 =       CoFlow startZ ejectZ feed1Z feed8Z
 where  
        startZ size
         = do   stateA  <- startA size
                stateB  <- startB size
                return  (stateA, stateB)

        ejectZ (stateA, stateB)
         = do   ejectA  stateA
                ejectB  stateB

        feed1Z  (stateA, stateB) snack1
         = do   feed1A stateA snack1
                feed1B stateB snack1

        feed8Z  (stateA, stateB) snack8
         = do   feed8A stateA snack8
                feed8B stateB snack8


