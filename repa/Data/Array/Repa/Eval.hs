{-# LANGUAGE UndecidableInstances #-}

-- | Low level interface to parallel array filling operators.
module Data.Array.Repa.Eval
        ( -- * Element types
          Elt       (..)

        -- * Parallel array filling
        , Fillable  (..)
        , Fill      (..)
        , FillRange (..)
        , fromList
        
        -- * Converting between representations
        , computeP, computeS
        , copyP,    copyS
        , now
        
        -- * Chunked filling
        , fillChunkedS
        , fillChunkedP
        , fillChunkedIOP

        -- * Blockwise filling
        , fillBlock2P
        , fillBlock2S
        
        -- * Cursored blockwise filling
        , fillCursoredBlock2S
        , fillCursoredBlock2P
        
        -- * Chunked selection
        , selectChunkedS
        , selectChunkedP)
where
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Eval.Fill
import Data.Array.Repa.Eval.Chunked
import Data.Array.Repa.Eval.Cursored
import Data.Array.Repa.Eval.Selection
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import System.IO.Unsafe


-- | Parallel computation of array elements.
--
--   * The `Fill` class is defined so that the source array must have a delayed representation (`D` or `C`)
--
--   * If you want to copy data between manifest representations then use `copy` instead.
--
--   * If you want to convert a manifest array back to a delayed representation then use `delay` instead.
--
computeP :: Fill r1 r2 sh e
        => Array r1 sh e -> Array r2 sh e
{-# INLINE computeP #-}
computeP arr1
 = unsafePerformIO
 $ do   marr2    <- newMArr (size $ extent arr1) 
        fillP arr1 marr2
        unsafeFreezeMArr (extent arr1) marr2


-- | Sequential computation of array elements.
computeS :: Fill r1 r2 sh e
        => Array r1 sh e -> Array r2 sh e
{-# INLINE computeS #-}
computeS arr1
 = unsafePerformIO
 $ do   marr2    <- newMArr (size $ extent arr1) 
        fillS arr1 marr2
        unsafeFreezeMArr (extent arr1) marr2




-- | Parallel copying of arrays.
--
--   * This is a wrapper that delays an array before calling `compute`. 
-- 
--   * You can use it to copy manifest arrays between representations.
--
--   * You can also use it to compute elements, but doing this may not be as efficient.
--     This is because delaying it the second time can hide information about
--     the structure of the original computation.
--
copyP   :: (Repr r1 e, Fill D r2 sh e)
        => Array r1 sh e -> Array r2 sh e
{-# INLINE copyP #-}
copyP arr1 = computeP $ delay arr1


-- | Sequential copying of arrays.
copyS   :: (Repr r1 e, Fill D r2 sh e)
        => Array r1 sh e -> Array r2 sh e
{-# INLINE copyS #-}
copyS arr1 = computeS $ delay arr1


        

-- | Apply `deepSeqArray` to an array so the result is actually constructed
--   at this point in a monadic computation. 
--
--   * Haskell's laziness means that applications of `compute` and `copy` are
--     automatically suspended.
--
--   * Laziness can be problematic for data parallel programs, because we want
--     each array to be constructed in parallel before moving onto the next one.
--   
now     :: (Shape sh, Repr r e, Monad m)
        => Array r sh e -> m (Array r sh e)
{-# INLINE now #-}
now arr
 = do   arr `deepSeqArray` return ()
        return arr
