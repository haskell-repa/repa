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
--   * The source array must have a delayed representation like `D`, `C` or `P`, 
--     and the result a manifest representation like `U` or `F`.
--
--   * If you want to copy data between manifest representations then use
--    `copyP` instead.
--
--   * If you want to convert a manifest array back to a delayed representation
--     then use `delay` instead.
--
computeP :: Fill r1 r2 sh e
        => Array r1 sh e -> Array r2 sh e
{-# INLINE [4] computeP #-}
computeP arr1
 = arr1 `deepSeqArray` 
   unsafePerformIO
 $ do   marr2    <- newMArr (size $ extent arr1) 
        fillP arr1 marr2
        unsafeFreezeMArr (extent arr1) marr2


-- | Sequential computation of array elements.
computeS 
        :: Fill r1 r2 sh e
        => Array r1 sh e -> Array r2 sh e
{-# INLINE [4] computeS #-}
computeS arr1
 = arr1 `deepSeqArray` 
   unsafePerformIO
 $ do   marr2    <- newMArr (size $ extent arr1) 
        fillS arr1 marr2
        unsafeFreezeMArr (extent arr1) marr2




-- | Parallel copying of arrays.
--
--   * This is a wrapper that delays an array before calling `computeP`. 
-- 
--   * You can use it to copy manifest arrays between representations.
--
--   * You can also use it to compute elements, but doing this may not be as
--     efficient. This is because delaying it the second time can hide
--     information about the structure of the original computation.
--
copyP   :: (Repr r1 e, Fill D r2 sh e)
        => Array r1 sh e -> Array r2 sh e
{-# INLINE [4] copyP #-}
copyP arr1 = computeP $ delay arr1


-- | Sequential copying of arrays.
copyS   :: (Repr r1 e, Fill D r2 sh e)
        => Array r1 sh e -> Array r2 sh e
{-# INLINE [4] copyS #-}
copyS arr1 = computeS $ delay arr1


        

-- | Apply `deepSeqArray` to an array so the result is actually constructed
--   at this point in a monadic computation. 
--
--   * Haskell's laziness means that applications of `computeP` and `copyP` are
--     automatically suspended.
--
--   * This is problematic for data parallel programs, because we want
--     each array to be constructed in parallel before moving onto the next one.
--
--   If you're not sure, then just use the following programming pattern:
--
--   @ someFunction arr1 arr2
--  = arr1 \`deepSeqArray\` arr2 \`deepSeqArray\`
--    do arr3 <- now $ computeP $ map f arr1
--       arr4 <- now $ computeP $ zipWith g arr3 arr2
--       return arr4
--   @
--
--  The @someFunction@ is your own function. Apply `deepSeqArray` to argument arrays, 
--  and apply `now` to arrays constructed with @compute@ or @copy@ functions. You can
--  do this in any state-like monad: `IO`, `ST` and @Control.Monad.State.Strict@ are all
--  good choices.
--
now     :: (Shape sh, Repr r e, Monad m)
        => Array r sh e -> m (Array r sh e)
{-# INLINE [4] now #-}
now arr
 = do   arr `deepSeqArray` return ()
        return arr
