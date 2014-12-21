
module Data.Repa.Flow.Gang.Base
        ( Sources (..), isWideSource
        , Sinks   (..), isWideSink)
where
import Data.Maybe

-- | An abstract gang of sources, with an integer index.
data Sources a
        = Sources
        { -- | How many streams are available from this source.
          --
          --   If `Nothing` then this is a wide sink that can provide as many
          --   streams as you want.
          sourcesArity  :: Maybe Int

          -- | Function to pull a new value from a source.
          --   If this returns Nothing then no more values will ever be available
          --   from that source, though there may be others from other source.
        , sourcesPull   :: (Int -> a -> IO ()) -> (Int -> IO ()) -> IO () }


-- | An abstract gang of sinks, with an integer index.
data Sinks a
        = Sinks
        { -- | How many streams can be pushed into this source.
          -- 
          --   If `Nothing` then this is a wide sink that can accept as many
          --   streams as you want.
          sinksArity    :: Maybe Int

          -- | Push an element into a sink.
        , sinksPush     :: Int -> a -> IO ()

          -- | Indicate that we've pushed all available elements into a sink.
        , sinksEject    :: Int -> IO () }


-- | Check if this source is wide, meaning it can provide as many streams
--   as you want.
isWideSource :: Sources a -> Bool
isWideSource (Sources ma _)     = isNothing ma
{-# INLINE isWideSource #-}


-- | Check if this sink is wide,   meaning it can accept as many streams
--   as you want.
isWideSink   :: Sinks a -> Bool
isWideSink   (Sinks ma _ _)     = isNothing ma
{-# INLINE isWideSink #-}

