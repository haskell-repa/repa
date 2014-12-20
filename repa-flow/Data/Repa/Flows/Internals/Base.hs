
module Data.Repa.Flows.Internals.Base
        ( Sources (..)
        , Sinks   (..))
where


-- | An abstract gang of sources, with an integer index.
data Sources a
        = Sources
        { -- How many source we've got.
          sourcesArity  :: Int

          -- | Function to pull a new value from a source.
          --   If this returns Nothing then no more values will ever be available
          --   from that source, though there may be others from other source.
        , sourcesPull   :: (Int -> a -> IO ()) -> (Int -> IO ()) -> IO () }


-- | An abstract gang of sinks, with an integer index.
data Sinks a
        = Sinks
        { -- | How many sinks we've got
          sinksArity    :: Int

          -- | Push an element into a sink.
        , sinksPush     :: Int -> a -> IO ()

          -- | Indicate that we've pushed all available elements into a sink.
        , sinksEject    :: Int -> IO () }
