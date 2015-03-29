
-- | Interface with stream fusion.
module Data.Repa.Eval.Stream
        ( streamOfArray
        , unstreamToArray
        , unstreamToArrayIO)
where
import Data.Repa.Fusion.Unpack
import Data.Repa.Array.Generic.Index                    as A
import Data.Repa.Array.Internals.Bulk                   as A
import Data.Repa.Array.Internals.Target                 as A
import qualified Data.Vector.Fusion.Stream              as SS
import qualified Data.Vector.Fusion.Stream.Monadic      as S
import qualified Data.Vector.Fusion.Stream.Size         as S
import qualified Data.Vector.Fusion.Util                as S
import System.IO.Unsafe
#include "repa-array.h"


-- | Produce a `Stream` for the elements of the given array.
streamOfArray  
        :: (Monad m, Bulk l a, Index l ~ Int)
        => A.Array  l a
        -> S.Stream m a

streamOfArray vec
        = S.generate (A.length vec)
                     (\i -> A.index vec i)
{-# INLINE_STREAM streamOfArray #-}


-------------------------------------------------------------------------------
-- | Compute the elements of a pure `Stream`,
--   writing them into a new array `Array`.
unstreamToArray
        :: (Target l a, Unpack (Buffer l a) t)
        => Name l -> S.Stream S.Id a -> Array l a

unstreamToArray nDst s
        = unsafePerformIO
        $ unstreamToArrayIO nDst
        $ SS.liftStream s
{-# INLINE_STREAM unstreamToArray #-}


-- | Compute the elements of an `IO` `Stream`,
--   writing them to a new `Array`.
unstreamToArrayIO
        :: (Target l a, Unpack (Buffer l a) t)
        => Name l -> S.Stream IO a -> IO (Array l a)

unstreamToArrayIO nDst (S.Stream step s0 sz)
 = case sz of
        S.Exact i       -> unstreamToArrayIO_max     i
        S.Max i         -> unstreamToArrayIO_max     i
        S.Unknown       -> unstreamToArrayIO_unknown 32

        -- unstream when we known the maximum size of the vector.
 where  unstreamToArrayIO_max !nMax
         = do   !vec0   <- unsafeNewBuffer  (create nDst zeroDim)
                !vec    <- unsafeGrowBuffer vec0 nMax

                let go_unstreamIO_max !sPEC !i !s
                     =  step s >>= \m
                     -> case m of
                         S.Yield e s'
                          -> do  unsafeWriteBuffer vec i e
                                 go_unstreamIO_max sPEC (i + 1) s'

                         S.Skip s'
                          ->     go_unstreamIO_max sPEC i s'

                         S.Done
                          -> do  buf'    <- unsafeSliceBuffer  0 i vec
                                 arr     <- unsafeFreezeBuffer buf'
                                 return arr
                    {-# INLINE_INNER go_unstreamIO_max #-}

                go_unstreamIO_max S.SPEC 0 s0
        {-# INLINE_INNER unstreamToArrayIO_max #-}

        -- unstream when we don't know the maximum size of the vector.
        unstreamToArrayIO_unknown !nStart
         = do   !vec0   <- unsafeNewBuffer  (create nDst zeroDim)
                !vec1   <- unsafeGrowBuffer vec0 nStart

                let go_unstreamIO_unknown !sPEC !uvec !i !n !s
                     = go_unstreamIO_unknown1 (repack vec0 uvec) i n s
                         (\vec' i' n' s' -> go_unstreamIO_unknown sPEC (unpack vec') i' n' s')
                         (\result        -> return result)

                    go_unstreamIO_unknown1 !vec !i !n !s cont done
                     =  step s >>= \r
                     -> case r of
                         S.Yield e s'
                          -> do (vec', n')
                                 <- if i >= n
                                        then do vec' <- unsafeGrowBuffer vec n
                                                return (vec', n + n)
                                        else    return (vec,  n)
                                unsafeWriteBuffer vec' i e
                                cont vec' (i + 1) n' s'

                         S.Skip s'
                          ->    cont vec i n s'

                         S.Done
                          -> do
                                vec' <- unsafeSliceBuffer  0 i vec
                                arr  <- unsafeFreezeBuffer vec'
                                done arr

                go_unstreamIO_unknown S.SPEC (unpack vec1) 0 nStart s0
        {-# INLINE_INNER unstreamToArrayIO_unknown #-}
{-# INLINE_STREAM unstreamToArrayIO #-}
