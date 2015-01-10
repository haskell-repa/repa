{-# LANGUAGE CPP #-}

-- | Evaluation of chains into bulk arrays.
module Data.Repa.Eval.Chain
        ( chainOfVector
        , unchainToVector
        , unchainToVectorIO)
where
import Data.Repa.Chain                 (Chain(..), Step(..))
import Data.Repa.Array.Internals.Bulk                   as R
import Data.Repa.Array.Internals.Target                 as R
import Data.Repa.Array.Internals.Index                  as R
import qualified Data.Repa.Chain                        as C
import qualified Data.Vector.Fusion.Stream.Monadic      as S
import qualified Data.Vector.Fusion.Stream.Size         as S
import qualified Data.Vector.Fusion.Util                as S
import Control.Monad.Identity
import System.IO.Unsafe
import GHC.Exts
import GHC.IO
#include "vector.h"


-------------------------------------------------------------------------------
-- | Produce a chain from a generic vector.
chainOfVector 
        :: (Monad m, Bulk r DIM1 a)
        => Vector r a -> Chain m Int a

chainOfVector !vec
 = Chain (S.Exact len) 0 step
 where
        !len  = R.length vec

        step !i
         | i >= len     = return $ Done  i
         | otherwise    = return $ Yield (R.index vec (Z :. i)) (i + 1)
        {-# INLINE step #-}
{-# INLINE [2] chainOfVector #-}


-- | Lift a pure chain to a monadic chain.
liftChain :: Monad m => Chain S.Id s a -> Chain m s a
liftChain (Chain sz s step)
        = Chain sz s (return . S.unId . step)
{-# INLINE  liftChain #-}


-------------------------------------------------------------------------------
-- | Compute the elements of a pure `Chain`,
--   writing them into a new array `Array`.
unchainToVector
        :: Target r a
        => Chain S.Id s a -> (Vector r a, s)
unchainToVector c
        = unsafePerformIO 
        $ unchainToVectorIO 
        $ liftChain c
{-# INLINE [2] unchainToVector #-}


-- | Compute the elements of an `IO` `Chain`, 
--   writing them to a new `Array`.
unchainToVectorIO
        :: forall r a s
        .  Target r a
        => Chain IO s a -> IO (Vector r a, s)

unchainToVectorIO (Chain sz s0 step)
 = case sz of
        S.Exact i       -> unchainToVectorIO_max     i 
        S.Max i         -> unchainToVectorIO_max     i 
        S.Unknown       -> unchainToVectorIO_unknown 32

        -- unchain when we known the maximum size of the vector.
 where  unchainToVectorIO_max !nMax
         = do   !vec     <- unsafeNewBuffer nMax

                let go_unchainIO_max !sPEC !i !s
                     =  step s >>= \m
                     -> case m of
                         Yield e s'    
                          -> do  unsafeWriteBuffer vec i e
                                 go_unchainIO_max sPEC (i + 1) s'
        
                         Skip s' 
                          ->     go_unchainIO_max sPEC i s'
        
                         Done s' 
                          -> do  vec'    <- unsafeSliceBuffer 0 i vec
                                 arr     <- unsafeFreezeBuffer (Z :. i) vec'
                                 return  (arr, s')
                    {-# INLINE go_unchainIO_max #-}

                go_unchainIO_max S.SPEC 0 s0
        {-# INLINE unchainToVectorIO_max #-}

        -- unchain when we don't know the maximum size of the vector.
        unchainToVectorIO_unknown !nStart
         = do   !vec0   <- unsafeNewBuffer nStart
                IO (go_unchainIO_unknown S.SPEC vec0 0 nStart s0)
         where  
                -- This consuming function has been desugared so that the recursion
                -- is via RealWorld, rather than using a function of type IO. 
                -- If the recursion is at IO then GHC tries to coerce to and from
                -- IO at every recursive call, which messes up SpecConstr.
                go_unchainIO_unknown 
                 :: S.SPEC -> Buffer r a -> Int -> Int -> s 
                 -> State# RealWorld -> (# State# RealWorld, (Array r DIM1 a, s) #)

                go_unchainIO_unknown !sPEC !vec !i !n !s !w0
                 = case unIO (step s) w0 of
                    (# w1, Yield e s' #)
                     | (# w2,  (vec', i', n') #)
                       <- unIO (do (vec', n') 
                                        <- if i >= n
                                            then do vec' <- unsafeGrowBuffer vec n
                                                    return (vec', n + n)
                                            else    return (vec,  n)
                                   unsafeWriteBuffer vec' i e
                                   return (vec', i + 1, n'))
                               w1
                     -> (go_unchainIO_unknown sPEC vec' i' n' s') w2

                    (# w1, Skip s' #)
                     -> (go_unchainIO_unknown sPEC vec  i  n  s') w1

                    (# w1, Done s' #)
                     -> (unIO $ do
                          vec' <- unsafeSliceBuffer 0 i vec
                          arr  <- unsafeFreezeBuffer (Z :. i) vec'
                          return (arr, s')) w1
                {-# INLINE go_unchainIO_unknown #-}
        {-# INLINE unchainToVectorIO_unknown #-}
{-# INLINE [2] unchainToVectorIO #-}

