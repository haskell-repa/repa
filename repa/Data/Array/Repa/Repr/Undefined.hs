
module Data.Array.Repa.Repr.Undefined
        ( X, Array (..))
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import Data.Array.Repa.Eval.Fill


-- | An array with undefined elements.
-- 
--   * This is normally used as the last representation in a partitioned array, 
--     as the previous partitions are expected to provide full coverage.
data X
data instance Array X sh e
        = AUndefined !sh


-- | Undefined array elements. Inspecting them yields `error`.
--
instance Repr X e where
 deepSeqArray _ x
        = x
 {-# INLINE deepSeqArray #-}

 extent (AUndefined sh) 
        = sh
 {-# INLINE extent #-}

 index (AUndefined _) _ 
        = error $ "Repa: array element is undefined."
 {-# INLINE index #-}
        
 linearIndex (AUndefined _) ix
        = error $ "Repa: array element at " ++ show ix ++ " is undefined."
 {-# INLINE linearIndex #-}
 

instance (Shape sh, Fillable r2 e, Num e) => Fill X r2 sh e where
 fillS _ _ = return ()
 fillP _ _ = return ()


