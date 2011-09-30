
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
        = AUndefined sh


-- | Read an undefined array elements. Inspecting their values yields `error`.
--
instance Repr X e where
 {-# INLINE extent #-}
 extent (AUndefined sh) 
        = sh

 {-# INLINE index #-}
 index (AUndefined _) _        = error "Repa: array element is undefined."
        
 {-# INLINE linearIndex #-}
 linearIndex (AUndefined _) _  = error "Repa: array element is undefined."
 

instance (Shape sh, Fillable r2 e, Num e) => Fill X r2 sh e where
 fillS _ _ = return ()
 fillP _ _ = return ()


