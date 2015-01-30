
module Data.Repa.Array.Internals.Layout
        (Layout  (..),  LayoutI)
where
import Data.Repa.Array.Internals.Shape


-- | A layout provides a total order on the elements of an index space.
--
--   We can talk about the n-th element of an array, independent of its
--   shape and dimensionality.
--
class Shape (Index l) => Layout l where

        -- | Short name for a layout which does not include details of
        --   the exact extent.
        data Name  l

        -- | Type used to index into this array layout.
        type Index l

        -- | O(1). Proxy for the layout name.
        name        :: Name l

        -- | O(1). Create a default layout of the given extent.
        create      :: Name l -> Index l -> l

        -- | O(1). Yield the extent of the layout.
        extent      :: l  -> Index l

        -- | O(1). Convert a polymorphic index to a linear one.
        toIndex     :: l  -> Index l -> Int

        -- | O(1). Convert a linear index to a polymorphic one.
        fromIndex   :: l  -> Int -> Index l


type LayoutI l  = (Layout l, Index l ~ Int)

