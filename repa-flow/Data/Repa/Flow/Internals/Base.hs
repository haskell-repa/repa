
module Data.Repa.Flow.Internals.Base
        ( Source (..)
        , Sink   (..))
where

-- | An abstract source of values.
--   We can pull values from this without knowing where they come from.
data Source a
        = Source
        { -- | Function to pull a new value from the source.
          ---  It is given a function to apply when elements are available,
          --   an action to invoke when we've hit the end of the flow.
          sourcePull    :: (a -> IO ()) -> IO () -> IO () }


-- | An abstract sink of values.
--   We can push values into this without knowing where they're going.
data Sink a
        = Sink
        { -- | Push a new element into the sink.
          sinkPush      :: a -> IO () 

          --   Indicate that we've pushed all available elements
          --   the sink should pass all contained information down stream.
        , sinkEject     :: IO () }
