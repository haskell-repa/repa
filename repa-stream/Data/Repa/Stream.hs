
module Data.Repa.Stream
        ( extractS
        , findSegmentsS
        , startLengthsOfSegsS

          -- * Unsafe operators
        , unsafeRatchetS)

where
import Data.Repa.Stream.Extract
import Data.Repa.Stream.Ratchet
import Data.Repa.Stream.Segment

