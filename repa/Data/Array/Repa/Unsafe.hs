
-- | Functions without sanity or bounds checks.
module Data.Array.Repa.Unsafe
        ( unsafeBackpermute
        , unsafeBackpermuteDft
        , unsafeSlice
        , unsafeExtend
        , unsafeTraverse
        , unsafeTraverse2
        , unsafeTraverse3
        , unsafeTraverse4)
where
import Data.Array.Repa.Operators.IndexSpace
import Data.Array.Repa.Operators.Traversal
