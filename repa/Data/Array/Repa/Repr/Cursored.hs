{-# LANGUAGE MagicHash #-}
module Data.Array.Repa.Repr.Cursored
        ( C, Array (..)
        , makeCursored)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import Data.Array.Repa.Index
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.Undefined
import Data.Array.Repa.Eval.Load
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Eval.Cursored
import Data.Array.Repa.Eval.Target
import GHC.Exts
import Debug.Trace

-- | Cursored Arrays.
--   These are produced by Repa's stencil functions, and help the fusion
--   framework to share index compuations between array elements.
--
--   The basic idea is described in ``Efficient Parallel Stencil Convolution'',
--   Ben Lippmeier and Gabriele Keller, Haskell 2011 -- though the underlying
--   array representation has changed since this paper was published.
data C


-- | Compute elements of a cursored array.
instance Source C a where

 data Array C sh a
        = forall cursor. ACursored
        { cursoredExtent :: !sh 
                
          -- | Make a cursor to a particular element.
        , makeCursor     :: sh -> cursor

          -- | Shift the cursor by an offset, to get to another element.
        , shiftCursor    :: sh -> cursor -> cursor

          -- | Load\/compute the element at the given cursor.
        , loadCursor     :: cursor -> a }


 index (ACursored _ makec _ loadc)
        = loadc . makec
 {-# INLINE index #-}

 unsafeIndex    = index
 {-# INLINE unsafeIndex #-}
 
 linearIndex (ACursored sh makec _ loadc)
        = loadc . makec . fromIndex sh
 {-# INLINE linearIndex #-}

 extent (ACursored sh _ _ _)
        = sh
 {-# INLINE extent #-}
        
 deepSeqArray (ACursored sh makec shiftc loadc) y
  = sh `deepSeq` makec  `seq` shiftc `seq` loadc `seq` y
 {-# INLINE deepSeqArray #-}


-- Fill -----------------------------------------------------------------------
-- | Compute all elements in an rank-2 array. 
instance Elt e => Load C DIM2 e where
 loadP (ACursored (Z :. (I# h) :. (I# w)) makec shiftc loadc) marr
  = do  traceEventIO "Repa.loadP[Cursored]: start"
        fillCursoredBlock2P 
                (unsafeWriteMVec marr) 
                makec shiftc loadc
                w 0# 0# w h
        touchMVec marr
        traceEventIO "Repa.loadP[Cursored]: end"
 {-# INLINE loadP #-}
        
 loadS (ACursored (Z :. (I# h) :. (I# w)) makec shiftc loadc) marr
  = do  traceEventIO "Repa.loadS[Cursored]: start"
        fillCursoredBlock2S 
                (unsafeWriteMVec marr) 
                makec shiftc loadc
                w 0# 0# w h
        touchMVec marr
        traceEventIO "Repa.loadS[Cursored]: end"
 {-# INLINE loadS #-}
        

-- | Compute a range of elements in a rank-2 array.
instance Elt e => LoadRange C DIM2 e where
 loadRangeP  (ACursored (Z :. _h :. (I# w)) makec shiftc loadc) marr
             (Z :. (I# y0) :. (I# x0)) (Z :. (I# h0) :. (I# w0))
  = do  traceEventIO "Repa.loadRangeP[Cursored]: start"
        fillCursoredBlock2P 
                (unsafeWriteMVec marr) 
                makec shiftc loadc
                w x0 y0 w0 h0
        touchMVec marr
        traceEventIO "Repa.loadRangeP[Cursored]: end"
 {-# INLINE loadRangeP #-}
        
 loadRangeS  (ACursored (Z :. _h :. (I# w)) makec shiftc loadc) marr
             (Z :. (I# y0) :. (I# x0)) 
             (Z :. (I# h0) :. (I# w0))
  = do  traceEventIO "Repa.loadRangeS[Cursored]: start"
        fillCursoredBlock2S
                (unsafeWriteMVec marr) 
                makec shiftc loadc
                w x0 y0 w0 h0
        touchMVec marr
        traceEventIO "Repa.loadRangeS[Cursored]: end"
 {-# INLINE loadRangeS #-}
        

-- Conversions ----------------------------------------------------------------
-- | Define a new cursored array.
makeCursored 
        :: sh
        -> (sh -> cursor)               -- ^ Create a cursor for an index.
        -> (sh -> cursor -> cursor)     -- ^ Shift a cursor by an offset.
        -> (cursor -> e)                -- ^ Compute the element at the cursor.
        -> Array C sh e

makeCursored = ACursored
{-# INLINE makeCursored #-}

