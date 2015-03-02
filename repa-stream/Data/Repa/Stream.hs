
module Data.Repa.Stream
        ( extractS
        , mergeS
        , findSegmentsS
        , diceSepS
        , startLengthsOfSegsS
        , padForwardS

          -- * Unsafe operators
        , unsafeRatchetS)

where
import Data.Repa.Stream.Extract
import Data.Repa.Stream.Merge
import Data.Repa.Stream.Ratchet
import Data.Repa.Stream.Segment
import Data.Repa.Stream.Dice
import Data.Repa.Stream.Pad

