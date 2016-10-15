
module Data.Array.Repa.Repr.Undefined
        ( X, Array (..))
where
import Data.Array.Repa.Base
import Data.Array.Repa.Shape
import Data.Array.Repa.Eval


-- | An array with undefined elements.
-- 
--   * This is normally used as the last representation in a partitioned array, 
--     as the previous partitions are expected to provide full coverage.
data X


-- | Undefined array elements. Inspecting them yields `error`.
--
instance Source X e where
 data Array X sh e
        = AUndefined !sh

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


deriving instance Show sh 
        => Show (Array X sh e)

deriving instance Read sh 
        => Read (Array X sh e)


instance Shape sh => Load X sh e where
 loadS _ _ = return ()
 loadP _ _ = return ()


