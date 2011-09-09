

module Data.Array.Repa.Repr.Partitioned
        ( P, Array (..)
        , Range(..)
        , inRange)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import Data.Array.Repa.Index
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.Undefined
import System.IO.Unsafe


-- | Partitioned arrays.
--   The last partition takes priority
data P r1 r2

data instance Array (P r1 r2) sh e
        = APart sh                              -- size of the whole array
                (Range sh) (Array r1 sh e)      -- if in range use this array
                (Array r2 sh e)                 -- otherwise use this array

data Range sh
        = Range sh sh                           -- indices defining the range
                (sh -> Bool)                    -- predicate to check whether were in range


instance (Repr r1 e, Repr r2 e) => Repr (P r1 r2) e where
 index (APart _ range arr1 arr2) ix
   | inRange range ix   = index arr1 ix
   | otherwise          = index arr2 ix

 extent (APart sh _ _ _) 
        = sh

 deepSeqArray (APart sh range arr1 arr2) y
  = sh `deepSeq` range `deepSeqRange` arr1 `deepSeqArray` arr2 `deepSeqArray` y


deepSeqRange :: Shape sh => Range sh -> b -> b
deepSeqRange (Range low high f) y
        = low `deepSeq` high `deepSeq` f `seq` y

-- | Check whether an index is within the given range.
inRange :: Range sh -> sh -> Bool
inRange (Range _ _ p) ix
        = p ix


-- Load2 ---------------------------------------------------------------------- 
-- | TODO: check that all of the array sizes match up.
instance Fill r e
      => Load2 (P D (P D X)) r e where
 {-# INLINE load2 #-}
 load2 (APart sh@(Z :. h :. w) (Range ix00 ix01 _) (ADelayed _ getElem1)
       (APart _                (Range ix10 ix11 _) (ADelayed _ getElem2)
        AUndefined))
  = unsafePerformIO 
  $ do  (marr :: MArr r e) <- newMArr (size sh) 
        fillBlock2P' (writeMArr marr) getElem1 w ix00 ix01
        fillBlock2P' (writeMArr marr) getElem2 w ix10 ix11
        unsafeFreezeMArr sh marr 


instance Fill r e
      => Load2 (P D (P D (P D (P D (P D X))))) r e where
 {-# INLINE load2 #-}
 load2 (APart sh@(Z :. h :. w) (Range ix00 ix01 _) (ADelayed _ getElem0)
       (APart _                (Range ix10 ix11 _) (ADelayed _ getElem1)
       (APart _                (Range ix20 ix21 _) (ADelayed _ getElem2)
       (APart _                (Range ix30 ix31 _) (ADelayed _ getElem3)
       (APart _                (Range ix40 ix41 _) (ADelayed _ getElem4)
        AUndefined)))))
  = unsafePerformIO 
  $ do  (marr :: MArr r e) <- newMArr (size sh) 
        fillBlock2P' (writeMArr marr) getElem0 w ix00 ix01
        fillBlock2P' (writeMArr marr) getElem1 w ix10 ix11
        fillBlock2P' (writeMArr marr) getElem2 w ix20 ix21
        fillBlock2P' (writeMArr marr) getElem3 w ix30 ix31
        fillBlock2P' (writeMArr marr) getElem4 w ix40 ix41
        unsafeFreezeMArr sh marr 


{-# INLINE fillBlock2P' #-}
fillBlock2P' update getElem w (Z :. y0 :. x0) (Z :. y1 :. x1)
 = fillBlock2P update getElem w y0 x0 y1 x1
 
 