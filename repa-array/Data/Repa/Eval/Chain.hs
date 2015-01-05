{-# LANGUAGE CPP #-}

-- | Evaluation of `Chain`s into bulk array.s
module Data.Repa.Eval.Chain
        (unchainToVector)
where
import Data.Repa.Array.Internals.Bulk                   as R
import Data.Repa.Array.Internals.Target                 as R
import Data.Repa.Array.Internals.Index                  as R

import Data.Repa.Chain                                  as C

import qualified Data.Vector.Fusion.Stream.Monadic      as S
import qualified Data.Vector.Fusion.Stream.Size         as S

import System.IO.Unsafe

#include "vector.h"


-- | Compute a `Chain` into an `Array`.
--
--   Chain computation is naturally sequential, so there can only be a
--   sequential version of this function.
--
unchainToVector
        :: Target r a
        => Chain a c  -> (Vector r a, c)
unchainToVector c
 = unsafePerformIO $ unchainToVectorIO c


-- | Compute a `Chain` into an `Array`.
unchainToVectorIO
        :: Target r a
        => Chain  a c -> IO (Vector r a, c)
unchainToVectorIO chain
 = case liftChain chain of
    Chain sz step s0 _start
     -> case sz of
         S.Exact i       -> unchainToVectorIO_max     i  step s0
         S.Max i         -> unchainToVectorIO_max     i  step s0
         S.Unknown       -> unchainToVectorIO_unknown 32 step s0
{-# INLINE_STREAM unchainToVectorIO #-}


-- unchain when we known the maximum size of the vector.
unchainToVectorIO_max nMax step s0
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
unchainToVectorIO_unknown nStart step s0
 =  unsafeNewBuffer nStart >>= \vec0
 -> let 
        go_unchainIO_unknown !sPEC !vec !i !n !s
         =  step s >>= \m
         -> case m of
                Yield e s'
                 | i >= n       
                 -> do  vec'    <- unsafeGrowBuffer vec n
                        unsafeWriteBuffer vec i e
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

