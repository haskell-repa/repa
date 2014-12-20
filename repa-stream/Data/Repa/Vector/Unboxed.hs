{-# LANGUAGE CPP #-}
module Data.Repa.Vector.Unboxed
        ( findSegments
        , findSegmentsFrom
        , ratchet
        , extract)
where
import Data.Repa.Stream.Segment
import Data.Vector.Unboxed                              (Unbox)
import qualified Data.Vector.Fusion.Stream.Monadic      as S
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector.Unboxed.Mutable            as UM
import qualified Data.Vector.Generic                    as G
import qualified Data.Vector.Generic.Mutable            as GM
import System.IO.Unsafe
import Data.IORef

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
--   @ratchet [(10,15), (20,26), (30,33), (40, 47)]
--     =  [10,20,30,40          -- 4
--        ,11,21,31,41          -- 4
--        ,12,22,32,42          -- 4
--        ,13,23   ,43          -- 3
--        ,14,24   ,44          -- 3
--           ,25   ,45          -- 2
--                 ,46]         -- 1@
--
--
ratchet :: U.Vector (Int, Int) -> (U.Vector Int, U.Vector Int)
ratchet vStartsMax 
 = unsafePerformIO
 $ do   
        -- Make buffers for the start values and unpack the max values.
        let (vStarts, vMax) = U.unzip vStartsMax
        mvStarts   <- U.thaw vStarts

        -- Make a vector for the output lengths.
        mvLens     <- UM.new (U.length vStartsMax)
        rmvLens    <- newIORef mvLens

        -- Run the computation
        mvStarts'  <- GM.munstream $ unsafeRatchetS mvStarts vMax rmvLens

        -- Read back the output segment lengths and freeze everything.
        mvLens'    <- readIORef rmvLens
        vStarts'   <- G.unsafeFreeze mvStarts'
        vLens'     <- G.unsafeFreeze mvLens'
        return (vStarts', vLens')


---------------------------------------------------------------------------------------------------
-- | Extraction.
extract :: Unbox a 
        => (Int -> a) -> U.Vector (Int, Int) -> U.Vector a
extract get vStartLen
 = G.unstream $ extractS get (G.stream vStartLen)
{-# INLINE_STREAM extract #-}


