
-- | * See the "Data.Repa.Vector.Unboxed" module for examples of how these
--     functions can be used.
module Data.Repa.Stream
        ( extractS
        , insertS
        , mergeS
        , compactS
        , compactInS
        , catMaybesS
        , findSegmentsS
        , diceSepS
        , startLengthsOfSegsS
        , padForwardS

          -- * Unsafe operators
        , unsafeRatchetS)

where
import Data.Repa.Stream.Concat
import Data.Repa.Stream.Compact
import Data.Repa.Stream.Dice
import Data.Repa.Stream.Extract
import Data.Repa.Stream.Insert
import Data.Repa.Stream.Merge
import Data.Repa.Stream.Pad
import Data.Repa.Stream.Ratchet
import Data.Repa.Stream.Segment

