
module Data.Array.Repa.Flow.Seq.CoFlow
        ( CoFlow (..)
        , Snack1 (..)
        , Snack8 (..))
where
import Data.Array.Repa.Flow.Seq.Base


-- | A 'CoFlow' is an abstract element consumer. We can push elements into
--   a coflow without knowing where they go.
--
--   Once the coflow is started, we feed elements into it and then call
--   the eject function that writes the result into the environment somewhere.
data CoFlow a
        = forall state. CoFlow
        { -- | Start the coflow, given the maximum number of elements
          --   we intend to feed to it. This returns a state value that
          --   needs to be passed to the other functions.
          -- 
          --   * Calling this more than once on a given coflow is undefined.
          --
          --   * Calling the other functions before doing this is undefined.
          coflowStart   :: Size -> IO state

          -- | Signal that we've fed the coflow all available elements.
          -- 
          --   This is done separately as from the feed functions so that they 
          --   don't need to check for end-of-input conditions on every iteration.
          --
          --   * Calling this more than once on a given coflow is undefined.
          --
          --   * Calling the feed functions after doing this is undefined.
        , coflowEject   :: state -> IO ()

          -- | Feed a single element to the coflow.
        , coflowFeed1   :: state -> Snack1 a -> IO ()

          -- | Feed eight elements to the coflow.
        , coflowFeed8   :: state -> Snack8 a -> IO () }


-- | Wraps an element to feed to a CoFlow.
data Snack1 a
        = Snack1 a

-- | Wraps eight elements to feed to a CoFlow.
data Snack8 a
        = Snack8 a a a a a a a a


