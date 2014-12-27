
module Data.Repa.Flow.Generic.Base
        ( module Data.Repa.Flow.States
        , Sources       (..)
        , Sinks         (..))
where
import Data.Repa.Flow.States


-- | A bundle of flow sources, indexed by a value of type @i@,
--   in some monad @m@, returning elements of type @e@.
--
--   Each source in the bundle can be pulled individually.
data Sources i m e
        = Sources
        { -- | Number of sources in this bundle.
          sourceArity   :: i

          -- | Function to pull data from a bundle. 
          --   Give it the index of the desired source, a continuation that 
          --   accepts an element, and a continuation to invoke when no more
          --   elements will ever be available.
        , sourcePull    :: Ix i -> (e -> m ()) -> m () -> m () }

-- | A bundle of flow sinks, indexed by a value of type @i@, 
--   in some monad @m@, returning elements of type @e@.
--
--   Each sink in the bundle can be pushed individually.
data Sinks   i m e
        = Sinks
        { -- | Number of sources in the bundle.
          sinkArity     :: i

          -- | Push an element to one of the sinks in the bundle.
        , sinkPush      :: Ix i -> e -> m ()

          -- | Signal that no more elements will ever be available for this sink.
        , sinkEject     :: Ix i -> m () }


