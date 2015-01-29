
module Data.Repa.Array.Index.Layout
        (Layout  (..))
where
import Data.Repa.Array.Index.Shape


-- | A layout represents the mapping from higher ranked indices to
--   linear indices, and determines what happens when an array is 
--   indexed out of bounds.
class Shape (Index lo) => Layout lo where

        type Index lo 
        -- TODO: add layout tag

        -- | O(1). Yield the extent of an array.
        --   For a 1-dimensional array this is equivalent to its length.
        extent      :: lo -> Index lo

        -- | Given an array shape and shape polymorphic index, 
        --   yield a linear index into an underlying, flat representation.
        toIndex     :: lo -> Index lo -> Int

        -- | Given an array shape and linear index,
        --   yield the shape polymorphic index.
        fromIndex   :: lo -> Int -> Index lo

