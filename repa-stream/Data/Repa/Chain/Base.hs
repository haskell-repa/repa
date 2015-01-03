{-# LANGUAGE CPP #-}
module Data.Repa.Chain.Base
        ( Step   (..)
        , MChain (..), Chain
        , liftChain
        , resume
        , fromStream
        , munchain)
where
import Control.Monad.Primitive
import Data.Vector.Generic.Mutable                      (MVector)
import qualified Data.Vector.Generic.Mutable            as GM
import qualified Data.Vector.Fusion.Stream.Monadic      as S
import qualified Data.Vector.Fusion.Stream.Size         as S
import qualified Data.Vector.Fusion.Util                as S

#include "vector.h"

-- | A Chain is similar to a Stream as used in stream fusion, except that
--   the Done step yields a continuation value so that the stream function
--   can be resumed when more source data is available.
--
--   Chain operations could instead be implemented with a combination of 
--   scan-left and filtering, but expressing with a modified stepper data 
--   type tends to be easier.
--
data MChain m a c
        = forall s. Chain 
        { mchainSize     :: S.Size 
        , mchainStep     :: s -> m (Step s a c)
        , mchainState    :: s 
        , mchainStart    :: c -> s }

data Step s a c
        = Yield a s     -- ^ A new element and a new seed.
        | Skip    s     -- ^ Just a new seed.
        | Done    c     -- ^ End of chunk, yield a continuation value. 


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


-- | Convert a Stream to a Chain that just yields unit when it's done.
fromStream
        :: Monad m 
        => c -> S.Stream m a -> MChain m a c
fromStream c (S.Stream istep s0 sz)
 = Chain sz ostep s0 (const s0)
 where
        ostep si
         =  istep si >>= \m
         -> case m of
                S.Yield e si'   -> return $ Yield e si'
                S.Skip    si'   -> return $ Skip    si'
                S.Done          -> return $ Done c
        {-# INLINE_INNER ostep #-}
{-# INLINE_STREAM fromStream #-}


-------------------------------------------------------------------------------
-- | Compute a chain into a mutable vector.
munchain :: (PrimMonad m, MVector v a)
         => MChain m a c
         -> m (v (PrimState m) a, c)

munchain (Chain sz step s0 _start)
 = case sz of
        S.Exact i       -> munchain_max     i  step s0
        S.Max i         -> munchain_max     i  step s0
        S.Unknown       -> munchain_unknown 32 step s0
{-# INLINE_STREAM munchain #-}


-- unchain when we known the maximum size of the vector.
munchain_max nMax step s0
 =  GM.unsafeNew nMax >>= \vec
 -> let go sPEC i s
         =  step s >>= \m
         -> case m of
                Yield e s'    
                 -> do  GM.unsafeWrite vec i e
                        go sPEC (i + 1) s'

                Skip s' -> go sPEC i s'
                Done s' -> return (GM.unsafeSlice 0 i vec, s')
        {-# INLINE_INNER go #-}

    in  go S.SPEC 0 s0
{-# INLINE_STREAM munchain_max #-}

-- unchain when we don't know the maximum size of the vector.
munchain_unknown nStart step s0
 =  GM.unsafeNew nStart >>= \vec0
 -> let go sPEC vec i n s
         =  step s >>= \m
         -> case m of
                Yield e s'
                 | i >= n       
                 -> do  vec'    <- GM.unsafeGrow vec n
                        GM.unsafeWrite vec i e
                        go sPEC vec' (i + 1) (n + n) s'

                 | otherwise
                 -> do  GM.unsafeWrite vec i e
                        go sPEC vec  (i + 1) n s'

                Skip s' -> go sPEC vec i n s'
                Done s' -> return (GM.unsafeSlice 0 i vec, s')
        {-# INLINE_INNER go #-}

    in go S.SPEC vec0 0 nStart s0
{-# INLINE_STREAM munchain_unknown #-}

