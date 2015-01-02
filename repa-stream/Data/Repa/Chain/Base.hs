{-# LANGUAGE CPP #-}
module Data.Repa.Chain.Base
        ( Step   (..)
        , MChain (..), Chain
        , liftChain
        , chainOfStream
        , mstart
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
data MChain m a c
        = forall s. Chain 
        { mchainSize     :: S.Size 
        , mchainStep     :: s -> m (Step s a c)
        , mchainState    :: s 
        , mchainResume   :: c -> s }

data Step s a c
        = Yield a s     -- ^ A new element and a new seed.
        | Skip    s     -- ^ Just a new seed.
        | Done    c     -- ^ End of chunk, yield a continuation value. 


-- | The type of pure chains.
type Chain = MChain S.Id


-- | Convert a pure chain to a monadic chain.
liftChain :: Monad m => Chain a c -> MChain m a c
liftChain (Chain sz step s resume)
        = Chain sz (return . S.unId . step) s resume
{-# INLINE_STREAM liftChain #-}


-- | Convert a Stream to a Chain that just yields unit when it's done.
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


---------------------------------------------------------------------------------------------------
mstart  :: (PrimMonad m, MVector v a)
        => MChain m a c 
        -> m (v (PrimState m) a, c)

mstart (Chain sz step s _resume)
 = case sz of
        S.Exact i       -> munchain_max     i  step s
        S.Max   i       -> munchain_max     i  step s
        S.Unknown       -> munchain_unknown 32 step s


-- | Compute a chain into a mutable vector.
munchain :: (PrimMonad m, MVector v a)
         =>   (MChain m a c,      c)
         -> m (v (PrimState m) a, c)

munchain (Chain sz step _s resume, c)
 = case sz of
        S.Exact i       -> munchain_max     i  step (resume c)
        S.Max i         -> munchain_max     i  step (resume c)
        S.Unknown       -> munchain_unknown 32 step (resume c)

-- unchain when we known the maximum size of the vector.
munchain_max nMax step s0
 =  GM.unsafeNew nMax >>= \vec
 -> let go i s
         =  step s >>= \m
         -> case m of
                Yield e s'    
                 -> do  GM.unsafeWrite vec i e
                        go (i + 1) s'

                Skip s' -> go i s'
                Done s' -> return (GM.unsafeSlice 0 i vec, s')

    in  go 0 s0


-- unchain when we don't know the maximum size of the vector.
munchain_unknown nStart step s0
 =  GM.unsafeNew nStart >>= \vec0
 -> let go vec i n s
         =  step s >>= \m
         -> case m of
                Yield e s'
                 | i >= n       
                 -> do  vec'    <- GM.unsafeGrow vec n
                        GM.unsafeWrite vec i e
                        go vec' (i + 1) (n + n) s'

                 | otherwise
                 -> do  GM.unsafeWrite vec i e
                        go vec  (i + 1) n s'

                Skip s' -> go vec i n s'
                Done s' -> return (GM.unsafeSlice 0 i vec, s')

    in go vec0 0 nStart s0

