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
import Data.Array.Repa.Eval.Fill
import Data.Array.Repa.Eval.Elt
import Data.Array.Repa.Eval.Cursored
import GHC.Exts

-- | Cursored Arrays
data C

data instance Array C sh e
        = forall cursor. ACursored
        { cursoredExtent :: sh 
                
          -- | Make a cursor to a particular element.
	, makeCursor    :: sh -> cursor

	  -- | Shift the cursor by an offset, to get to another element.
	, shiftCursor   :: sh -> cursor -> cursor

	  -- | Load\/compute the element at the given cursor.
	, loadCursor	:: cursor -> e }


-- Repr -----------------------------------------------------------------------
-- | Compute elements of a cursored array.
instance Repr C a where
 {-# INLINE index #-}
 index (ACursored _ makec _ loadc)
        = loadc . makec

 {-# INLINE unsafeIndex #-}
 unsafeIndex    = index
 
 {-# INLINE linearIndex #-}
 linearIndex (ACursored sh makec _ loadc)
        = loadc . makec . fromIndex sh

 {-# INLINE extent #-}
 extent (ACursored sh _ _ _)
        = sh
        
 {-# INLINE deepSeqArray #-}
 deepSeqArray (ACursored sh makec shiftc loadc) y
  = sh `deepSeq` makec `seq` shiftc `seq` loadc `seq` y


-- Fill -----------------------------------------------------------------------
-- | Compute all elements in an rank-2 array. 
instance (Fillable r2 e, Elt e) => Fill C r2 DIM2 e where
 {-# INLINE fillP #-}
 fillP (ACursored (Z :. h :. w) makec shiftc loadc) marr
  = fillCursoredBlock2P 
                (unsafeWriteMArr marr) 
                makec shiftc loadc
                w 0 0 (w - 1) (h - 1) 

 {-# INLINE fillS #-}
 fillS (ACursored (Z :. (I# h) :. (I# w)) makec shiftc loadc) marr
  = fillCursoredBlock2S 
                (unsafeWriteMArr marr) 
                makec shiftc loadc
                w 0# 0# (w -# 1#) (h -# 1#) 


-- | Compute a range of elements in a rank-2 array.
instance (Fillable r2 e, Elt e) => FillRange C r2 DIM2 e where
 {-# INLINE fillRangeP #-}
 fillRangeP  (ACursored (Z :. _h :. w) makec shiftc loadc) marr
             (Z :. y0 :. x0) (Z :. y1 :. x1)
  = fillCursoredBlock2P 
                (unsafeWriteMArr marr) 
                makec shiftc loadc
                w x0 y0 x1 y1

 {-# INLINE fillRangeS #-}
 fillRangeS  (ACursored (Z :. _h :. (I# w)) makec shiftc loadc) marr
             (Z :. (I# y0) :. (I# x0)) 
             (Z :. (I# y1) :. (I# x1))
  = fillCursoredBlock2S
                (unsafeWriteMArr marr) 
                makec shiftc loadc
                w x0 y0 x1 y1
 
-- Conversions ----------------------------------------------------------------
-- | Define a new cursored array.
makeCursored 
        :: sh
        -> (sh -> cursor)               -- ^ Create a cursor for an index.
        -> (sh -> cursor -> cursor)     -- ^ Shift a cursor by an offset.
        -> (cursor -> e)                -- ^ Compute the element at the cursor.
        -> Array C sh e

{-# INLINE makeCursored #-}
makeCursored = ACursored
