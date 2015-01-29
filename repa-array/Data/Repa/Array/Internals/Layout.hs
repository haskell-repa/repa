
module Data.Repa.Array.Internals.Layout
        (Layout  (..))
where
import Data.Repa.Array.Internals.Shape


-- | A Layout specifies the physical representation of an array,
--   and gives a total order to the elements contained within.
class Shape (Index lo) => Layout lo where

        -- | Short name for a layout which does not include details of
        --   the exact size.
        data Name  lo   

        -- | Type used to index into this array layout.
        type Index lo 

        -- | O(1). Create a default layout that will hold the given 
        --   number of elements.
        create      :: Name lo -> Int -> lo

        -- | O(1). Yield the extent of an array.
        --   For a 1-dimensional array this is equivalent to its length.
        extent      :: lo  -> Index lo

        -- | O(1). Given an array shape and shape polymorphic index, 
        --   yield a linear index into the underlying flat representation.
        toIndex     :: lo  -> Index lo -> Int

        -- | O(1). Given an array shape and linear index,
        --   yield the shape polymorphic index.
        fromIndex   :: lo  -> Int -> Index lo

