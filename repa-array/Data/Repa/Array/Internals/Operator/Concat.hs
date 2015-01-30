{-# LANGUAGE CPP #-}

-- | Concatenation operators on arrays.
module Data.Repa.Array.Internals.Operator.Concat
        ( concat
        , concatWith
        , intercalate
        , ConcatDict)
where
import Data.Repa.Array.Material                         as A
import Data.Repa.Array.Delayed                          as A
import Data.Repa.Array.Index                            as A
import Data.Repa.Array.Internals.Target                 as A
import Data.Repa.Array.Internals.Bulk                   as A
import Data.Repa.Eval.Array                             as A
import Data.Repa.Fusion.Unpack                          as A
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector.Fusion.Stream.Monadic      as V
import System.IO.Unsafe
import GHC.Exts hiding (fromList, toList)
import Prelude  hiding (reverse, length, map, zipWith, concat)
#include "repa-array.h"


-- | Dictionaries needed to perform a concatenation.
type ConcatDict lOut lIn tIn lDst a
      = ( BulkI   lOut (Array lIn a)
        , BulkI   lIn a
        , TargetI lDst a
        , Unpack (Array lIn a) tIn)


-------------------------------------------------------------------------------
-- | O(len result) Concatenate nested arrays.
--
-- @
-- > import Data.Repa.Array.Material
-- > let arrs = fromList B [fromList U [1, 2, 3], fromList U [5, 6, 7 :: Int]]
-- > toList $ concat U arrs
-- [1,2,3,5,6,7]
-- @
--  
concat  :: ConcatDict lOut lIn tIn lDst a
        => Name  lDst                   -- ^ Layout for destination.
        -> Array lOut (Array lIn a)     -- ^ Arrays to concatenate.
        -> Array lDst a
concat nDst vs
 | A.length vs == 0
 = A.fromList nDst []

 | otherwise
 = unsafePerformIO
 $ do   let !lens  = toUnboxed $ computeS U $ A.map A.length vs
        let !len   = U.sum lens
        !buf_      <- unsafeNewBuffer  (create nDst 0)
        !buf       <- unsafeGrowBuffer buf_ len
        let !iLenY = U.length lens

        let loop_concat !iO !iY !row !iX !iLenX
             | iX >= iLenX
             = if iY >= iLenY - 1
                then return ()
                else let iY'    = iY + 1
                         row'   = vs `index` iY'
                         iLenX' = A.length row'
                     in  loop_concat iO iY' row' 0 iLenX'

             | otherwise
             = do let x = row `index` iX
                  unsafeWriteBuffer buf iO x
                  loop_concat (iO + 1) iY row (iX + 1) iLenX
            {-# INLINE_INNER loop_concat #-}

        let !row0   = vs `index` 0
        let !iLenX0 = A.length row0
        loop_concat 0 0 row0 0 iLenX0

        unsafeFreezeBuffer buf
{-# INLINE_ARRAY concat #-}


-------------------------------------------------------------------------------
-- | O(len result) Concatenate the elements of some nested vector,
--   inserting a copy of the provided separator array between each element.
--
-- @
-- > import Data.Repa.Array.Material
-- > let sep  = fromList U [0, 0, 0]
-- > let arrs = fromList B [fromList U [1, 2, 3], fromList U [5, 6, 7 :: Int]]
-- > toList $ concatWith U sep arrs
-- [1,2,3,0,0,0,5,6,7,0,0,0]
-- @
--
concatWith
        :: ( ConcatDict lOut lIn tIn lDst a
           , BulkI   lSep a)
        => Name lDst                  -- ^ Result representation.
        -> Array lSep a               -- ^ Separator array.
        -> Array lOut (Array lIn a)   -- ^ Arrays to concatenate.
        -> Array lDst a

concatWith nDst !is !vs
 | A.length vs == 0
 = A.fromList nDst []

 | otherwise
 = unsafePerformIO
 $ do   
        -- Lengths of the source vectors.
        let !lens       = toUnboxed $ computeS U $ A.map A.length vs

        -- Length of the final result vector.
        let !(I# len)   = U.sum lens
                        + U.length lens * A.length is

        -- New buffer for the result vector.
        !buf_           <- unsafeNewBuffer  (create nDst 0)
        !buf            <- unsafeGrowBuffer buf_ (I# len)
        let !(I# iLenY) = U.length lens
        let !(I# iLenI) = A.length is
        let !row0       = vs `index` 0

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
                 let !row'        = vs `index` (I# iY')
                 let !(I# iLenX') = A.length row'
                 loop_concatWith sPEC iO' iY' (unpack row') 0# iLenX'

             -- Keep copying a source element.
             | otherwise
             = do let x = (repack row0 row) `index` (I# iX)
                  unsafeWriteBuffer buf (I# iO) x
                  loop_concatWith sPEC (iO +# 1#) iY row (iX +# 1#) iLenX
            {-# INLINE_INNER loop_concatWith #-}

            -- Inject the separator array.
            loop_concatWith_inject !sPEC !iO !n
             | 1# <- n >=# iLenI = return (I# iO)
             | otherwise
             = do let x = is `index` (I# n)
                  unsafeWriteBuffer buf (I# iO) x
                  loop_concatWith_inject sPEC (iO +# 1#) (n +# 1#)
            {-# INLINE_INNER loop_concatWith_inject #-}

        let !(I# iLenX0) = A.length row0
        loop_concatWith V.SPEC 0# 0# (unpack row0) 0# iLenX0
        unsafeFreezeBuffer buf
{-# INLINE_ARRAY concatWith #-}



-- Intercalate ----------------------------------------------------------------
-- | O(len result) Insert a copy of the separator array between the elements of
--   the second and concatenate the result.
--
-- @
-- > import Data.Repa.Array.Material
-- > let sep  = fromList U [0, 0, 0]
-- > let arrs = fromList B [fromList U [1, 2, 3], fromList U [5, 6, 7 :: Int]]
-- > toList $ intercalate U sep arrs
-- [1,2,3,0,0,0,5,6,7]
-- @
--
intercalate 
        :: ( ConcatDict lOut lIn tIn lDst a
           , BulkI   lSep a)
        => Name lDst                  -- ^ Result representation.
        -> Array lSep a               -- ^ Separator array.
        -> Array lOut (Array lIn a)   -- ^ Arrays to concatenate.
        -> Array lDst a

intercalate nDst !is !vs
 | A.length vs == 0
 = A.fromList nDst []

 | otherwise
 = unsafePerformIO
 $ do   
        -- Lengths of the source vectors.
        let !lens       = toUnboxed $ computeS U $ A.map A.length vs

        -- Length of the final result vector.
        let !(I# len)   = U.sum lens
                        + (U.length lens - 1) * A.length is

        -- New buffer for the result vector.
        !buf_           <- unsafeNewBuffer (create nDst 0)
        !buf            <- unsafeGrowBuffer buf_ (I# len)
        let !(I# iLenY) = U.length lens
        let !(I# iLenI) = A.length is
        let !row0       = vs `index` 0

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
                 let !row'        = vs `index` (I# iY')
                 let !(I# iLenX') = A.length row'
                 loop_intercalate sPEC iO' iY' (unpack row') 0# iLenX'

             -- Keep copying a source element.
             | otherwise
             = do let x = (repack row0 row) `index` (I# iX)
                  unsafeWriteBuffer buf (I# iO) x
                  loop_intercalate sPEC (iO +# 1#) iY row (iX +# 1#) iLenX
            {-# INLINE_INNER loop_intercalate #-}

            -- Inject the separator array.
            loop_intercalate_inject !sPEC !iO !n
             | 1# <- n >=# iLenI = return (I# iO)
             | otherwise
             = do let x = is `index` (I# n)
                  unsafeWriteBuffer buf (I# iO) x
                  loop_intercalate_inject sPEC (iO +# 1#) (n +# 1#)
            {-# INLINE_INNER loop_intercalate_inject #-}

        let !(I# iLenX0) = A.length row0
        loop_intercalate V.SPEC 0# 0# (unpack row0) 0# iLenX0
        unsafeFreezeBuffer buf
{-# INLINE_ARRAY intercalate #-}

