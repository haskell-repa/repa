{-# LANGUAGE CPP #-}

-- | Concatenation operators on arrays.
module Data.Repa.Array.Internals.Operator.Concat
        ( concat
        , concatWith
        , intercalate )
where
import Data.Repa.Eval.Array                     as R
import Data.Repa.Array.Delayed                  as R
import Data.Repa.Array.Unboxed                  as R
import Data.Repa.Array.Internals.Index          as R
import Data.Repa.Array.Internals.Target         as R
import Data.Repa.Array.Internals.Bulk           as R
import System.IO.Unsafe
import qualified Data.Vector.Unboxed            as U
import Prelude  hiding (reverse, length, map, zipWith, concat)
import GHC.Exts hiding (fromList, toList)

import Data.Repa.Array.Foreign                  as R
import Foreign.ForeignPtr
import qualified Data.Vector.Fusion.Stream.Monadic as V
import Prelude  hiding (concat)

#include "vector.h"


-- Concat ---------------------------------------------------------------------
-- | O(len result) Concatenate nested vectors.
concat  :: (Bulk r1 DIM1 (Vector r2 a), Bulk r2 DIM1 a, Target r3 a)
        => Vector r1 (Vector r2 a) -> Vector r3 a
concat vs
 | R.length vs == 0
 = R.vfromList []

 | otherwise
 = unsafePerformIO
 $ do   let !lens  = toVectorU $ computeS $ R.map R.length vs
        let !len   = U.sum lens
        !buf       <- unsafeNewBuffer len
        let !iLenY = U.length lens

        let loop_concat !iO !iY !row !iX !iLenX
             | iX >= iLenX
             = if iY >= iLenY - 1
                then return ()
                else let iY'    = iY + 1
                         row'   = vs `index` (Z :. iY')
                         iLenX' = R.length row'
                     in  loop_concat iO iY' row' 0 iLenX'

             | otherwise
             = do let x = row `index` (Z :. iX)
                  unsafeWriteBuffer buf iO x
                  loop_concat (iO + 1) iY row (iX + 1) iLenX
            {-# INLINE loop_concat #-}

        let !row0   = vs `index` (Z :. 0)
        let !iLenX0 = R.length row0
        loop_concat 0 0 row0 0 iLenX0

        unsafeFreezeBuffer (Z :. len) buf
{-# INLINE [2] concat #-}


-- O(len result) Concatenate the elements of some nested vector,
-- inserting a copy of the provided separator array between each element.
concatWith
        :: ( Bulk r0 DIM1 a
           , Bulk r1 DIM1 (Vector r2 a)
           , Bulk r2 DIM1 a, Unpack1 (Vector r2 a) t
           , Target r3 a)
        => Vector r0 a                  -- ^ Separator array.
        -> Vector r1 (Vector r2 a)      -- ^ Elements to concatenate.
        -> Vector r3 a

concatWith !is !vs
 | R.length vs == 0
 = R.vfromList []

 | otherwise
 = unsafePerformIO
 $ do   
        -- Lengths of the source vectors.
        let !lens       = toVectorU $ computeS $ R.map R.length vs

        -- Length of the final result vector.
        let !(I# len)   = U.sum lens
                        + U.length lens * R.length is

        -- New buffer for the result vector.
        !buf            <- unsafeNewBuffer (I# len)
        let !(I# iLenY) = U.length lens
        let !(I# iLenI) = R.length is
        let !row0       = vs `index` (Z :. 0)

        let loop_concatWith !sPEC !iO !iY !row !iX !iLenX
             -- We've finished copying one of the source elements.
             | 1# <- iX >=# iLenX
             = case iY >=# iLenY -# 1# of

                -- We've finished all of the source elements.
                1# -> do
                 _      <- loop_concatWith_inject sPEC iO 0#
                 return ()

                -- We've finished one of the source elements, but it wasn't
                -- the last one. Inject the separator array then copy the 
                -- next element.
                _  -> do

                 -- TODO: We're probably getting an unboxing an reboxing
                 --       here. Check the fused code.
                 I# iO' <- loop_concatWith_inject sPEC iO 0#
                 let !iY'         = iY +# 1#
                 let !row'        = vs `index` (Z :. I# iY')
                 let !(I# iLenX') = R.length row'
                 loop_concatWith sPEC iO' iY' (unpack row') 0# iLenX'

             -- Keep copying a source element.
             | otherwise
             = do let x = (repack row0 row) `index` (Z :. I# iX)
                  unsafeWriteBuffer buf (I# iO) x
                  loop_concatWith sPEC (iO +# 1#) iY row (iX +# 1#) iLenX
            {-# INLINE_INNER loop_concatWith #-}

            -- Inject the separator array.
            loop_concatWith_inject !sPEC !iO !n
             | 1# <- n >=# iLenI = return (I# iO)
             | otherwise
             = do let x = is `index` (Z :. I# n)
                  unsafeWriteBuffer buf (I# iO) x
                  loop_concatWith_inject sPEC (iO +# 1#) (n +# 1#)
            {-# INLINE_INNER loop_concatWith_inject #-}

        let !(I# iLenX0) = R.length row0
        loop_concatWith V.SPEC 0# 0# (unpack row0) 0# iLenX0
        unsafeFreezeBuffer (Z :. (I# len)) buf
{-# INLINE [2] concatWith #-}


-- Intercalate ----------------------------------------------------------------
-- O(len result) Insert a copy of the separator array between the elements of
-- the second and concatenate the result.
intercalate 
        :: ( Bulk r0 DIM1 a
           , Bulk r1 DIM1 (Vector r2 a)
           , Bulk r2 DIM1 a, Unpack1 (Vector r2 a) t
           , Target r3 a)
        => Vector r0 a                  -- ^ Separator array.
        -> Vector r1 (Vector r2 a)      -- ^ Elements to concatenate.
        -> Vector r3 a

intercalate !is !vs
 | R.length vs == 0
 = R.vfromList []

 | otherwise
 = unsafePerformIO
 $ do   
        -- Lengths of the source vectors.
        let !lens       = toVectorU $ computeS $ R.map R.length vs

        -- Length of the final result vector.
        let !(I# len)   = U.sum lens
                        + (U.length lens - 1) * R.length is

        -- New buffer for the result vector.
        !buf            <- unsafeNewBuffer (I# len)
        let !(I# iLenY) = U.length lens
        let !(I# iLenI) = R.length is
        let !row0       = vs `index` (Z :. 0)

        let loop_intercalate !sPEC !iO !iY !row !iX !iLenX
             -- We've finished copying one of the source elements.
             | 1# <- iX >=# iLenX
             = case iY >=# iLenY -# 1# of

                -- We've finished all of the source elements.
                1# -> return ()

                -- We've finished one of the source elements, but it wasn't
                -- the last one. Inject the separator array then copy the 
                -- next element.
                _  -> do

                 -- TODO: We're probably getting an unboxing an reboxing
                 --       here. Check the fused code.
                 I# iO'           <- loop_intercalate_inject sPEC iO 0#
                 let !iY'         = iY +# 1#
                 let !row'        = vs `index` (Z :. I# iY')
                 let !(I# iLenX') = R.length row'
                 loop_intercalate sPEC iO' iY' (unpack row') 0# iLenX'

             -- Keep copying a source element.
             | otherwise
             = do let x = (repack row0 row) `index` (Z :. I# iX)
                  unsafeWriteBuffer buf (I# iO) x
                  loop_intercalate sPEC (iO +# 1#) iY row (iX +# 1#) iLenX
            {-# INLINE_INNER loop_intercalate #-}

            -- Inject the separator array.
            loop_intercalate_inject !sPEC !iO !n
             | 1# <- n >=# iLenI = return (I# iO)
             | otherwise
             = do let x = is `index` (Z :. I# n)
                  unsafeWriteBuffer buf (I# iO) x
                  loop_intercalate_inject sPEC (iO +# 1#) (n +# 1#)
            {-# INLINE_INNER loop_intercalate_inject #-}

        let !(I# iLenX0) = R.length row0
        loop_intercalate V.SPEC 0# 0# (unpack row0) 0# iLenX0
        unsafeFreezeBuffer (Z :. (I# len)) buf
{-# INLINE [2] intercalate #-}


-------------------------------------------------------------------------------
-- | Unpack the pieces of a structure into a tuple.
--
--   This is used in a low-level fusion optimisation to ensure that
--   intermediate values are unboxed.
--
class Unpack1 a t | a -> t where
 unpack :: a -> t
 repack :: a -> t -> a

instance Unpack1 (Array F DIM1 a) (Int, Int, ForeignPtr a) where
 unpack (FArray (Z :. len) offset fptr) = (len, offset, fptr)
 repack _ (len, offset, fptr)           = FArray (Z :. len) offset fptr

