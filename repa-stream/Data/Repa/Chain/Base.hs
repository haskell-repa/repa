{-# LANGUAGE CPP #-}
module Data.Repa.Chain.Base
        ( Step  (..)
        , Chain (..)
        , liftChain
        , resumeChain)
where
import Control.Monad.Primitive
import Data.Vector.Generic.Mutable                      (MVector)
import qualified Data.Vector.Generic                    as GV
import qualified Data.Vector.Generic.Mutable            as GM
import qualified Data.Vector.Fusion.Stream.Monadic      as S
import qualified Data.Vector.Fusion.Stream.Size         as S
import qualified Data.Vector.Fusion.Util                as S
#include "vector.h"


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
        = Yield a s

        -- | Provide just a new seed.
        | Skip    s

        -- | Signal that the computation has finished.
        | Done    s
        deriving Show 


-- | Lift a pure chain to a monadic chain.
liftChain :: Monad m => Chain S.Id s a -> Chain m s a
liftChain (Chain sz s step)
        = Chain sz s (return . S.unId . step)
{-# INLINE_STREAM liftChain #-}


-- | Resume a chain computation from a previous state.
resumeChain  
        :: Monad m 
        => s -> Chain m s a -> Chain m s a
resumeChain s' (Chain sz _s step)
 = Chain sz s' step
{-# INLINE_STREAM resumeChain #-}

