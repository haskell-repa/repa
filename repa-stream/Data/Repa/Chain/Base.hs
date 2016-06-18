
module Data.Repa.Chain.Base
        ( Step  (..)
        , Chain (..)
        , liftChain
        , resumeChain)
where
import qualified Data.Vector.Fusion.Stream.Size         as S
import Control.Monad.Identity
#include "repa-stream.h"


-- | A chain is an abstract, stateful producer of elements. It is similar
--   a stream as used in stream fusion, except that internal state is visible
--   in its type. This allows the computation to be paused and resumed at a
--   later point.
data Chain m s a
        = Chain 
        { -- | Expected size of the output.
          mchainSize     :: S.Size 

          -- | Starting state.
        , mchainState    :: s 

          -- | Step the chain computation.
        , mchainStep     :: s -> m (Step s a) }


-- | Result of a chain computation step.
data Step s a

        -- | Yield an output value and a new seed.
        = Yield !a !s

        -- | Provide just a new seed.
        | Skip     !s

        -- | Signal that the computation has finished.
        | Done     !s
        deriving Show 


-- | Lift a pure chain to a monadic chain.
liftChain :: Monad m => Chain Identity s a -> Chain m s a
liftChain (Chain sz s step)
        = Chain sz s (return . runIdentity . step)
{-# INLINE_STREAM liftChain #-}


-- | Resume a chain computation from a previous state.
resumeChain :: s -> Chain m s a -> Chain m s a
resumeChain s' (Chain sz _s step)
 = Chain sz s' step
{-# INLINE_STREAM resumeChain #-}

