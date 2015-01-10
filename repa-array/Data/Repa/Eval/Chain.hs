{-# LANGUAGE CPP #-}

-- | Evaluation of `Chain`s into bulk array.
module Data.Repa.Eval.Chain
        ( chainOfVector
        , unchainToVector)
where
import Data.Repa.Chain                 (Chain(..), Step(..))
import Data.Repa.Array.Internals.Bulk                   as R
import Data.Repa.Array.Internals.Target                 as R
import Data.Repa.Array.Internals.Index                  as R
import qualified Data.Repa.Chain                        as C
import qualified Data.Vector.Fusion.Stream.Monadic      as S
import qualified Data.Vector.Fusion.Stream.Size         as S
import System.IO.Unsafe
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
         | i >= len
         = return $ Done  i

         | otherwise    
         = return $ Yield (R.index vec (Z :. i)) (i + 1)
        {-# INLINE_INNER step #-}
{-# INLINE_STREAM chainOfVector #-}


-------------------------------------------------------------------------------
-- | Compute the elements of a `Chain`, writing them into a new array `Array`.
--
--   Chain computation is naturally sequential, so there can only be a
--   sequential version of this function.
--
unchainToVector
        :: Target r a
        => Chain C.Id s a -> (Vector r a, s)
unchainToVector c
        = unsafePerformIO 
        $ unchainToVectorIO 
        $ C.liftChain c


-- | Compute a `Chain` into an `Array`.
unchainToVectorIO
        :: Target r a
        => Chain  s a -> IO (Vector r a, s)

unchainToVectorIO chain
 = case C.liftChain chain of
    Chain sz s0 step
     -> case sz of
         S.Exact i       -> unchainToVectorIO_max     i  s0 step 
         S.Max i         -> unchainToVectorIO_max     i  s0 step
         S.Unknown       -> unchainToVectorIO_unknown 32 s0 step
{-# INLINE_STREAM unchainToVectorIO #-}


-- unchain when we known the maximum size of the vector.
unchainToVectorIO_max nMax s0 step
 =  unsafeNewBuffer nMax >>= \vec
 -> let 
        go_unchainIO_max !sPEC !i !s
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
        {-# INLINE_INNER go_unchainIO_max #-}

    in  go_unchainIO_max S.SPEC 0 s0
{-# INLINE_STREAM unchainToVectorIO_max #-}


-- unchain when we don't know the maximum size of the vector.
unchainToVectorIO_unknown nStart s0 step 
 =  unsafeNewBuffer nStart >>= \vec0
 -> let 
        go_unchainIO_unknown !sPEC !vec !i !n !s
         =  step s >>= \m
         -> case m of
                Yield e s'
                 | i >= n       
                 -> do  vec'    <- unsafeGrowBuffer vec n
                        unsafeWriteBuffer vec' i e
                        go_unchainIO_unknown sPEC vec' (i + 1) (n + n) s'

                 | otherwise
                 -> do  unsafeWriteBuffer vec i e
                        go_unchainIO_unknown sPEC vec  (i + 1) n s'

                Skip s' 
                 ->     go_unchainIO_unknown sPEC vec i n s'

                Done s' 
                 -> do  vec'    <- unsafeSliceBuffer 0 i vec
                        arr     <- unsafeFreezeBuffer (Z :. i) vec'
                        return  (arr, s')
        {-# INLINE_INNER go_unchainIO_unknown #-}

    in go_unchainIO_unknown S.SPEC vec0 0 nStart s0
{-# INLINE_STREAM unchainToVectorIO_unknown #-}

