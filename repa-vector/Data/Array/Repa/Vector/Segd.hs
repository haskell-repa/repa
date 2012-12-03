
module Data.Array.Repa.Vector.Segd
        ( -- * Segment Descriptors
          Segd
        , empty
        , fromLengths
        , lengths
        , elements

          -- * Split Segment Descriptors
        , SplitSegd     (..)
        , splitSegd
        , unsplitSegd)

where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Unboxed
import Data.Array.Repa.Flow.Par.Segd            (Segd, SplitSegd, empty)
import qualified Data.Array.Repa.Flow.Par.Segd  as Segd
import qualified Data.Vector.Unboxed            as U
import GHC.Exts


-- | Construct a segment descriptor from a vector of segment lengths.
fromLengths :: Vector U Int -> Segd
fromLengths vec
        = Segd.fromLengths (toUnboxed vec)
{-# INLINE [4] fromLengths #-}


-- | Take the segment lengths of a segment descriptor.
lengths :: Segd -> Vector U Int
lengths segd
        = fromUnboxed (Z :. U.length (Segd.lengths segd)) 
        $ Segd.lengths segd
{-# INLINE [4] lengths #-}


-- | Take the total number of elements covered by a segment descriptor.
elements :: Segd -> Int
elements segd
        = I# (Segd.elements segd)
{-# INLINE [4] elements #-}


-- | O(log segs). Preprocess a `Segd` by splitting it into chunks, trying
--   to assign the same number of elements to each thread.
splitSegd   :: Segd -> SplitSegd
splitSegd segd
        = Segd.splitSegd theGang segd


-- | O(1). Take the original unsplit version from a SplitSegd.
unsplitSegd :: SplitSegd -> Segd
unsplitSegd segd
        = Segd.splitOriginal segd
{-# INLINE [4] unsplitSegd #-}
