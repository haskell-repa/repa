
module Data.Array.Repa.Repr.Undefined
        ( X, Array (..))
where
import Data.Array.Repa.Base

-- | An array with undefined elements.
-- 
--   * This is normally used as the last representation in a partitioned array, 
--     as the previous partitions are expected to provide full coverage.
data X
data instance Array X sh e
        = AUndefined
