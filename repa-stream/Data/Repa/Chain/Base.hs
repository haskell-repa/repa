{-# LANGUAGE CPP #-}
module Data.Repa.Chain.Base
        ( Step   (..)
        , MChain (..), Chain
        , liftChain
        , resume
        , chainOfStream
        , unchainToMVector)
where
import Control.Monad.Primitive
import Data.Vector.Generic.Mutable                      (MVector)
import qualified Data.Vector.Generic.Mutable            as GM
import qualified Data.Vector.Fusion.Stream.Monadic      as S
import qualified Data.Vector.Fusion.Stream.Size         as S
import qualified Data.Vector.Fusion.Util                as S

#include "vector.h"

-- | A Monadic Chain is similar to a Monadic Stream as used in stream fusion,
--   except that the Done step yields a continuation value which allows a chain
--   computation to be resumed when more source data is available.
--
--   Chain operations could instead be implemented with a combination of 
--   scan-left and filtering, but expressing with a modified stepper data 
--   type tends to be easier.
--
data MChain m a c
        = forall s. Chain 
        { -- | Expected size of the output.
          mchainSize     :: S.Size 

          -- | Step the chain computation.
        , mchainStep     :: s -> m (Step s a c)

          -- | Internal state used during computation.
        , mchainState    :: s 

          -- | Convert a continuation value to a new starting state.
        , mchainStart    :: c -> s }


-- | Result of a chain computation step.
data Step s a c
        = Yield a s     -- ^ A new element and a new seed.
        | Skip    s     -- ^ Just a new seed.
        | Done    c     -- ^ Signal end of input, and yield a continuation value. 
                        --   so this computation can be resumed.


-- | The type of pure chains.
type Chain = MChain S.Id


-- | Convert a pure chain to a monadic chain.
liftChain :: Monad m => Chain a c -> MChain m a c
liftChain (Chain sz step s start)
        = Chain sz (return . S.unId . step) s start
{-# INLINE_STREAM liftChain #-}


-- | Resume a chain computation from a previous state.
resume  :: Monad m 
        => c -> MChain m a c -> MChain m a c
resume c (Chain sz step _s start)
 = Chain sz step (start c) start
{-# INLINE_STREAM resume #-}


-- | Convert a stream to a chain that yields unit when it's done.
chainOfStream
        :: Monad m 
        => c -> S.Stream m a -> MChain m a c
chainOfStream c (S.Stream istep s0 sz)
 = Chain sz ostep s0 (const s0)
 where
        ostep si
         =  istep si >>= \m
         -> case m of
                S.Yield e si'   -> return $ Yield e si'
                S.Skip    si'   -> return $ Skip    si'
                S.Done          -> return $ Done c
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM chainOfStream #-}


-------------------------------------------------------------------------------
-- | Compute a chain into a mutable vector.
unchainToMVector
        :: (PrimMonad m, MVector v a)
        => MChain m a c
        -> m (v (PrimState m) a, c)

unchainToMVector (Chain sz step s0 _start)
 = case sz of
        S.Exact i       -> unchainToMVector_max     i  step s0
        S.Max i         -> unchainToMVector_max     i  step s0
        S.Unknown       -> unchainToMVector_unknown 32 step s0
{-# INLINE_STREAM unchainToMVector #-}


-- unchain when we known the maximum size of the vector.
unchainToMVector_max nMax step s0
 =  GM.unsafeNew nMax >>= \vec
 -> let go !sPEC !i !s
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
unchainToMVector_unknown nStart step s0
 =  GM.unsafeNew nStart >>= \vec0
 -> let go !sPEC !vec !i !n !s
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

