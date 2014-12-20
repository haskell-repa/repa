{-# LANGUAGE CPP #-}
module Data.Repa.Vector.Unboxed
        ( findSegments
        , findSegmentsFrom
        , ratchet)
where
import Data.Repa.Stream.Segment
import qualified Data.Vector.Fusion.Stream.Monadic      as S
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector.Generic                    as G
import qualified Data.Vector.Generic.Mutable            as GM
import System.IO.Unsafe

#include "vector.h"


---------------------------------------------------------------------------------------------------
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


---------------------------------------------------------------------------------------------------
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


---------------------------------------------------------------------------------------------------
-- | Interleaved `enumFromTo`. 
--
--   Given a vector of pairs of starting and stopping values, produce a vector
--   where we increase each of the starting values to the stopping values in
--   a round-robin order. 
--
--   @ratchet [(10,14), (20,25), (30,32), (40, 46)]
--     =  [10,20,30,40
--        ,11,21,31,41
--        ,12,22,32,42
--        ,13,23   ,43
--        ,14,24   ,44
--           ,25   ,45
--                 ,46]@
--
--
ratchet :: U.Vector (Int, Int) -> U.Vector Int
ratchet vStartsMax 
 = unsafePerformIO
 $ do   
        let (vStarts, vMax) = U.unzip vStartsMax
        mvec    <- U.thaw vStarts
        mvec'   <- GM.munstream $ unsafeRatchetS mvec vMax
        G.unsafeFreeze mvec'



