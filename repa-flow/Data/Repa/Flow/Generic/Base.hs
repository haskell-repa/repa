
module Data.Repa.Flow.Generic.Base
        ( module Data.Repa.Flow.States
        , Sources       (..)
        , Sinks         (..)
        , reSource)
where
import Data.Repa.Flow.States


data Sources i m e
        = Sources
        { sourceArity   :: i
        , sourcePull    :: i -> (e -> m ()) -> m () -> m () }


data Sinks   i m e
        = Sinks
        { sinkArity     :: i
        , sinkPush      :: i -> e -> m ()
        , sinkEject     :: i -> m () }


-------------------------------------------------------------------------------
reSource :: (i1 -> i2) 
         -> (i2 -> i1)
         -> Sources i1 m a -> Sources i2 m a
reSource from to (Sources n1 pullX)
 = Sources (from n1) pull_reSource
 where  pull_reSource i2 pull eject 
         = pullX (to i2) pull eject
        {-# INLINE pull_reSource #-}
{-# INLINE [2] reSource #-}
        
        
