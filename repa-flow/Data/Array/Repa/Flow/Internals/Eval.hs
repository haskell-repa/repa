
module Data.Array.Repa.Flow.Internals.Eval
        (drain)
where
import Data.Array.Repa.Flow.Internals.Base


-- | Pull all available values from the source and push them to the sink.
drain :: Source a -> Sink a -> IO ()
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
