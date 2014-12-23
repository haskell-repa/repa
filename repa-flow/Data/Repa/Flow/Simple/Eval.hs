
module Data.Repa.Flow.Simple.Eval
        (drain)
where
import Data.Repa.Flow.Simple.Base


-- | Pull all available values from the source and push them to the sink.
drain :: Monad m => Source m a -> Sink m a -> m ()
drain (Source ipull) (Sink opush oeject)
 = loop_drain
 where 
       loop_drain 
        = ipull eat_drain eject_drain

       eat_drain v
        = opush v >> loop_drain

       eject_drain
        = oeject
{-# INLINE [1] drain #-}
