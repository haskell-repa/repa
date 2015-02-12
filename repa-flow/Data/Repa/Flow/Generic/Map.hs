
module Data.Repa.Flow.Generic.Map
        (smap_i,       smap_o)
where
import Data.Repa.Flow.Generic.Base
import Control.Monad
import Prelude                                  as P
#include "repa-flow.h"


-- | Apply a function to every element pulled from some sources, 
--   producing some new sources. The worker function is also given
--   the stream index.
smap_i  :: Monad m
        => (i -> a -> b) -> Sources i m a -> m (Sources i m b)
smap_i f (Sources n pullsA)
 = return $ Sources n pullsB_map
 where  
        pullsB_map i eat eject
         = pullsA  i eat_a eject_a
         where  
                eat_a v = eat (f i v)
                {-# INLINE eat_a #-}

                eject_a = eject
                {-# INLINE eject_a #-}

        {-# INLINE [1] pullsB_map #-}
{-# INLINE_FLOW smap_i #-}


-- | Apply a function to every element pushed to some sink,
--   producing a new sink. The worker function is also given
--   the stream index.
smap_o   :: Monad m
        => (i -> a -> b) -> Sinks i m b -> m (Sinks i m a)
smap_o f (Sinks n pushB ejectB)
 = return $ Sinks n pushA_map ejectA_map
 where  
        pushA_map i a   = pushB  i (f i a)
        {-# INLINE pushA_map #-}

        ejectA_map i    = ejectB i
        {-# INLINE ejectA_map #-}
{-# INLINE_FLOW smap_o #-}


