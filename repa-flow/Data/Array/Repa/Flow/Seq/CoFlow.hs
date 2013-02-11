
module Data.Array.Repa.Flow.Seq.CoFlow
        ( CoFlow (..)
        , Snack1 (..)
        , Snack8 (..))
where
import Data.Array.Repa.Flow.Seq.Base


-- | A 'CoFlow' is an abstract data sink for a given element type.
--   Once the flow is started, we feed elements into it and then call
--   the eject function that writes the result into the environment somewhere.
data CoFlow a
        = forall state. CoFlow
        { -- | Start the coflow, given the maximum number of elements
          --   we intend to feed to it. This returns a state value that
          --   needs to be passed to the other functions.
          -- 
          --   * Calling this more than once on a given coflow is undefined.
          coflowStart   :: Size -> IO state

          -- | Signal that we've fed the coflow all available elements.
          -- 
          --
          --   * Calling this more than once on a given coflow is undefined.
          --
          --   * Calling the feed functions after doing this is undefined.
        , coflowEject   :: state -> IO ()

          -- | Try to feed a single element to the coflow.
          --   Returns a flag saying whether it accepted the element.
        , coflowFeed1   :: state -> Snack1 a -> IO Bool

          -- | Try to feed eight elements to the coflow.
          --   Returns a flag saying whether it accepted the elements.
        , coflowFeed8   :: state -> Snack8 a -> IO Bool }


-- | Wraps an element to feed to a CoFlow.
data Snack1 a
        = Snack1 a

-- | Wraps eight elements to feed to a CoFlow.
data Snack8 a
        = Snack8 a a a a a a a a


