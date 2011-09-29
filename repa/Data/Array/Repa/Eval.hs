{-# LANGUAGE UndecidableInstances #-}

-- | Low level interface to parallel array filling operators.
module Data.Array.Repa.Eval
        ( Elt       (..)
        , Fillable  (..)
        , Fill      (..)
        , FillRange (..)
        
        -- * Loading array data between representations
        , compute, copy
        
        -- * The Gang
        , Gang, seqGang, forkGang, gangSize, gangIO, gangST, theGang
        
        -- * Chunked filling
        , fillChunkedS
        , fillChunkedP

        -- * Blockwise filling
        , fillBlock2P
        
        -- * Cursored blockwise filling
        , fillCursoredBlock2S
        , fillCursoredBlock2P)
where
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Eval.Gang
import Data.Array.Repa.Eval.Fill
import Data.Array.Repa.Eval.Chunked
import Data.Array.Repa.Eval.Cursored
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import System.IO.Unsafe


-- | Compute array elements in parallel.
--
--   * The `Fill` class is defined so that the source array must have a delayed representation (`D` or `C`)
--
--   * If you want to copy data between manifest representations then use `copy` instead.
--
--   * If you want to convert a manifest array back to a delayed representation then use `delay` instead.
--
compute :: Fill r1 r2 sh e
        => Array r1 sh e -> Array r2 sh e
{-# INLINE compute #-}
compute arr1
 = unsafePerformIO
 $ do   marr2    <- newMArr (size $ extent arr1) 
        fillP arr1 marr2
        unsafeFreezeMArr (extent arr1) marr2


-- | Copying of arrays.
--
--   * This is a wrapper that delays an array before calling `compute`. 
-- 
--   * You can use it to copy manifest arrays between representations.
--
--   * You can also use it to compute elements, but doing this may not be as efficient.
--     This is because delaying it the second time can hide information about
--     the structure of the original computation.
--
copy    :: (Repr r1 e, Fill D r2 sh e)
        => Array r1 sh e -> Array r2 sh e
copy arr1
        = compute $ delay arr1