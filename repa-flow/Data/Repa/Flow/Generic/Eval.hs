
module Data.Repa.Flow.Generic.Eval
        (drainS)
where
import Data.Repa.Flow.Generic.Base
#include "repa-stream.h"


-- | Pull all available values from the sources and push them to the sinks.
drainS   :: (Index i, Monad m)
        => Sources i m a -> Sinks i m a -> m ()

drainS (Sources nSources ipull) (Sinks nSinks opush oeject)
 = loop_drain (zero n)
 where 
        n = min nSources nSinks

        loop_drain !ix
         = ipull ix eat_drain eject_drain
         where  eat_drain  v
                 = opush ix v >> loop_drain ix
                {-# INLINE eat_drain #-}

                eject_drain
                 = do   oeject ix  
                        case next ix of
                         Nothing        -> return ()
                         Just ix'       -> loop_drain ix'
                {-# INLINE eject_drain #-}
        {-# INLINE loop_drain #-}
{-# INLINE_FLOW drainS #-}
