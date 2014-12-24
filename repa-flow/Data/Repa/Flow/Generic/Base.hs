
module Data.Repa.Flow.Generic.Base
        ( module Data.Repa.Flow.States
        , Sources       (..)
        , Sinks         (..))
where
import Data.Repa.Flow.States


data Sources i m e
        = Sources
        { sourceArity   :: i
        , sourcePull    :: Ix i -> (e -> m ()) -> m () -> m () }


data Sinks   i m e
        = Sinks
        { sinkArity     :: i
        , sinkPush      :: Ix i -> e -> m ()
        , sinkEject     :: Ix i -> m () }
