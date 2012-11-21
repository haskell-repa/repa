
module Data.Array.Repa.Vector.Segd
        ( -- * Segment Descriptors
          Segd
        , empty
        , fromLengths
        , lengths

          -- * Split Segment Descriptors
        , SplitSegd
        , splitSegd)

where
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Vector.Repr.Unboxed
import Data.Array.Repa.Flow.Par.Segd            hiding (fromLengths, lengths)
import qualified Data.Array.Repa.Flow.Par.Segd  as Segd
import qualified Data.Vector.Unboxed            as U


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
