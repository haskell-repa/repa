
module Data.Repa.Array.Internals.Layout
        (Layout  (..))
where
import Data.Repa.Array.Internals.Shape


-- | A layout provides a total order on the elements of an index space.
--
--   We can talk about the n-th element of an array, independent of its
--   shape and dimensionality.
--
class Shape (Index lo) => Layout lo where

        -- | Short name for a layout which does not include details of
        --   the exact extent.
        data Name  lo   

        -- | Type used to index into this array layout.
        type Index lo 

        -- | O(1). Create a default layout of the given extent.
        create      :: Name lo -> Index lo -> lo

        -- | O(1). Yield the extent of the layout.
        extent      :: lo  -> Index lo

        -- | O(1). Convert a polymorphic index to a linear one.
        toIndex     :: lo  -> Index lo -> Int

        -- | O(1). Convert a linear index to a polymorphic one.
        fromIndex   :: lo  -> Int -> Index lo

