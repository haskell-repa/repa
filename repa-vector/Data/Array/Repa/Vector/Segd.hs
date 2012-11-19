
module Data.Array.Repa.Vector.Segd
        ( -- * Segment Descriptors
          Segd
        , empty
        , fromLengths

          -- * Split Segment Descriptors
        , SplitSegd
        , splitSegd)

where
import Data.Array.Repa
import Data.Array.Repa.Vector.Base
import Data.Array.Repa.Flow.Par.Segd            hiding (fromLengths)
import qualified Data.Array.Repa.Flow.Par.Segd  as Segd


-- | Construct a segment descriptor from a vector of segment lengths.
fromLengths :: Vector U Int -> Segd
fromLengths vec
        = Segd.fromLengths (toUnboxed vec)
{-# INLINE [4] fromLengths #-}
