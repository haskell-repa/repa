{-# LANGUAGE CPP #-}
module Data.Repa.Chain.Base
        ( Step   (..)
        , MChain (..), Chain
        , liftChain
        , resume
        , chainOfVector
        , unchainToMVector)
where
import Control.Monad.Primitive
import Data.Vector.Generic.Mutable                      (MVector)
import qualified Data.Vector.Generic                    as GV
import qualified Data.Vector.Generic.Mutable            as GM
import qualified Data.Vector.Fusion.Stream.Monadic      as S
import qualified Data.Vector.Fusion.Stream.Size         as S
import qualified Data.Vector.Fusion.Util                as S
#include "vector.h"


-- | A Monadic Chain is similar to a Monadic Stream as used in stream fusion,
--   except that the internal state is visible in its type. This allows the
--   computation to be paused and resumed at a later point.
--
--   Chain operations could instead be implemented with a combination of 
--   scan-left and filtering, but expressing with a modified stepper data 
--   type tends to be easier.
--
data MChain m s a
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

        -- | Signal end of output because the computation has finished.
        --   This alternative should be used for chain generators like
        --   `replicate`, where the output has a fixed length.
        | Done    s     


-- | The type of pure chains.
type Chain = MChain S.Id


-- | Convert a pure chain to a monadic chain.
liftChain :: Monad m => Chain s a -> MChain m s a
liftChain (Chain sz s step)
        = Chain sz s (return . S.unId . step)
{-# INLINE_STREAM liftChain #-}


-- | Resume a chain computation from a previous state.
resume  :: Monad m 
        => s -> MChain m s a -> MChain m s a
resume s' (Chain sz _s step)
 = Chain sz s' step
{-# INLINE_STREAM resume #-}


-------------------------------------------------------------------------------
-- | Produce a chain from a generic vector.
chainOfVector 
        :: GV.Vector v a
        => v a -> Chain Int a

chainOfVector vec
 = Chain (S.Exact len) 0 step
 where
        !len  = GV.length vec

        step !i
         | i >= len
         = return $ Done  i

         | otherwise    
         = return $ Yield (GV.unsafeIndex vec i) (i + 1)
        {-# INLINE_INNER step #-}
{-# INLINE_STREAM chainOfVector #-}


-------------------------------------------------------------------------------
-- | Compute a chain into a mutable vector.
unchainToMVector
        :: (PrimMonad m, MVector v a)
        => MChain m s a
        -> m (v (PrimState m) a, s)

unchainToMVector (Chain sz s0 step)
 = case sz of
        S.Exact i       -> unchainToMVector_max     i  s0 step
        S.Max i         -> unchainToMVector_max     i  s0 step
        S.Unknown       -> unchainToMVector_unknown 32 s0 step
{-# INLINE_STREAM unchainToMVector #-}


-- unchain when we known the maximum size of the vector.
unchainToMVector_max nMax s0 step 
 =  GM.unsafeNew nMax >>= \vec
 -> let 
        go !sPEC !i !s
         =  step s >>= \m
         -> case m of
                Yield e s'
                 -> do  GM.unsafeWrite vec i e
                        go sPEC (i + 1) s'

                Skip s' -> go sPEC i s'
                Done s' -> return (GM.unsafeSlice 0 i vec, s')
        {-# INLINE_INNER go #-}

    in  go S.SPEC 0 s0
{-# INLINE_STREAM unchainToMVector_max #-}


-- unchain when we don't know the maximum size of the vector.
unchainToMVector_unknown nStart s0 step
 =  GM.unsafeNew nStart >>= \vec0
 -> let 
        go !sPEC !vec !i !n !s
         =  step s >>= \m
         -> case m of
                Yield e s'
                 | i >= n       
                 -> do  vec'    <- GM.unsafeGrow vec n
                        GM.unsafeWrite vec' i e
                        go sPEC vec' (i + 1) (n + n) s'

                 | otherwise
                 -> do  GM.unsafeWrite vec i e
                        go sPEC vec  (i + 1) n s'

                Skip s' -> go sPEC vec i n s'
                Done s' -> return (GM.unsafeSlice 0 i vec, s')
        {-# INLINE_INNER go #-}

    in go S.SPEC vec0 0 nStart s0
{-# INLINE_STREAM unchainToMVector_unknown #-}

