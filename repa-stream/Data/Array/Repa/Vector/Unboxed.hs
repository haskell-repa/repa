{-# LANGUAGE CPP #-}
module Data.Array.Repa.Vector.Unboxed
        ( findSegments
        , findSegmentsFrom)
where
import Data.Array.Repa.Stream.Segment
import qualified Data.Vector.Fusion.Stream.Monadic      as S
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector.Generic                    as G

#include "vector.h"


-- | Given predicates that detect the beginning and end of some interesting
--   segment of information, scan through a vector looking for when these
--   segments begin and end. Return vectors of the segment starting positions
--   and lengths.
findSegments 
        :: U.Unbox a 
        => (a -> Bool)          -- ^ Predicate to check for start of segment.
        -> (a -> Bool)          -- ^ Predicate to check for end of segment.
        ->  U.Vector a          -- ^ Input vector.
        -> (U.Vector Int, U.Vector Int)

findSegments pStart pEnd src
        = U.unzip
        $ G.unstream
        $ startLengthsOfSegsS
        $ findSegmentsS pStart pEnd (U.length src)
        $ S.indexed 
        $ G.stream src
{-# INLINE_STREAM findSegments #-}



-- | Given predicates that detect the beginning and end of some interesting
--   segment of information, scan through a vector looking for when these
--   segments begin and end. Return vectors of the segment starting positions
--   and lengths.
findSegmentsFrom
        :: (a -> Bool)          -- ^ Predicate to check for start of segment.
        -> (a -> Bool)          -- ^ Predicate to check for end of segment.
        -> Int                  -- ^ Input length.
        -> (Int -> a)           -- ^ Get an element from the input.
        -> (U.Vector Int, U.Vector Int)

findSegmentsFrom pStart pEnd len get
        = U.unzip
        $ G.unstream
        $ startLengthsOfSegsS
        $ findSegmentsS pStart pEnd len
        $ S.map         (\ix -> (ix, get ix))
        $ S.enumFromStepN 0 1 len
{-# INLINE_STREAM findSegmentsFrom #-}
