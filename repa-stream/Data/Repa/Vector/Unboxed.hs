{-# LANGUAGE CPP #-}
module Data.Repa.Vector.Unboxed
        ( -- * Conversion
          chainOfVector
        , unchainToVectorIO
        , unchainToVectorM

          -- * Operators
        , scanMaybe
        , groupsBy
        , findSegments
        , findSegmentsFrom
        , ratchet
        , extract

          -- ** Folds
        , foldsT, C.Folds(..))
where
import Data.Repa.Stream.Extract
import Data.Repa.Stream.Ratchet
import Data.Repa.Stream.Segment

import qualified Data.Repa.Chain                        as C

import Data.Vector.Unboxed                              (Unbox)
import qualified Data.Vector.Unboxed                    as U
import qualified Data.Vector.Unboxed.Mutable            as UM
import qualified Data.Vector.Generic                    as G
import qualified Data.Vector.Generic.Mutable            as GM
import qualified Data.Vector.Fusion.Stream.Monadic      as S

import Control.Monad.ST
import Control.Monad.Primitive
import System.IO.Unsafe
import Data.IORef

#include "vector.h"

-------------------------------------------------------------------------------
chainOfVector     
        :: (Monad m, Unbox a)
        => U.Vector a -> C.MChain m Int a
chainOfVector
 = C.chainOfVector

unchainToVectorIO 
        :: Unbox a 
        => C.MChain IO s a -> (U.Vector a, s)
unchainToVectorIO chain
 = unsafePerformIO $ unchainToVectorM chain
{-# INLINE_STREAM unchainToVectorIO #-}


-- | Compute a monadic chain, producing a vector of elements.
unchainToVectorM
        :: (PrimMonad m, Unbox a)
        => C.MChain m s a  -> m (U.Vector a, s)
unchainToVectorM chain
 = do   (mvec, c') <- C.unchainToMVector chain
        vec        <- U.unsafeFreeze mvec
        return (vec, c')
{-# INLINE_STREAM unchainToVectorM #-}


-------------------------------------------------------------------------------
-- | Perform a left-to-right scan through an input vector, maintaining
--   a state value between each element.
scanMaybe 
        :: (Unbox a, Unbox b)
        => (s -> a -> (s, Maybe b)) 
        ->  s
        ->  U.Vector a 
        -> (U.Vector b, s)

scanMaybe f k0 vec0
 = (vec1, snd k1)
 where  
        f' s x = return $ f s x

        (vec1, k1)
         = runST $ unchainToVectorM    $ C.liftChain 
                 $ C.scanMaybeC f' k0  $ C.chainOfVector vec0


{-# INLINE_STREAM scanMaybe #-}


-------------------------------------------------------------------------------
-- | From a stream of values which has consecutive runs of idential values,
--   produce a stream of the lengths of these runs.
-- 
-- @
--  groupsBy (==) (Just ('a', 4)) 
--                [\'a\', \'a\', \'a\', \'b\', \'b\', \'c\', \'d\', \'d\'] 
--   => ([('a', 7), ('b', 2), ('c', 1)], Just (\'d\', 2))
-- @
--
groupsBy
        :: Unbox a
        => (a -> a -> Bool)             -- ^ Comparison function.
        -> Maybe (a, Int)               -- ^ Starting element and count.
        ->  U.Vector a                  -- ^ Input elements.
        -> (U.Vector (a, Int), Maybe (a, Int))

groupsBy f !c !vec0
 = (vec1, snd k1)
 where  
        f' x y = return $ f x y

        (vec1, k1)
         = runST $ unchainToVectorM $ C.liftChain 
                 $ C.groupsByC f' c $ C.chainOfVector vec0
{-# INLINE_STREAM groupsBy #-}


-------------------------------------------------------------------------------
-- | Given predicates that detect the beginning and end of some interesting
--   segment of information, scan through a vector looking for when these
--   segments begin and end. Return vectors of the segment starting positions
--   and lengths.
--
--   * As each segment must end on a element where the ending predicate returns
--     True, the miniumum segment length returned is 1.
--
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
        $ findSegmentsS pStart pEnd (U.length src - 1)
        $ S.indexed 
        $ G.stream src
{-# INLINE_STREAM findSegments #-}


-------------------------------------------------------------------------------
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
        $ findSegmentsS pStart pEnd (len - 1)
        $ S.map         (\ix -> (ix, get ix))
        $ S.enumFromStepN 0 1 len
{-# INLINE_STREAM findSegmentsFrom #-}


-------------------------------------------------------------------------------
-- | Interleaved `enumFromTo`. 
--
--   Given a vector of starting values, and a vector of stopping values, 
--   produce an stream of elements where we increase each of the starting
--   values to the stopping values in a round-robin order. Also produce a
--   vector of result segment lengths.
--
-- @
--  unsafeRatchetS [10,20,30,40] [15,26,33,47]
--  =  [10,20,30,40       -- 4
--     ,11,21,31,41       -- 4
--     ,12,22,32,42       -- 4
--     ,13,23   ,43       -- 3
--     ,14,24   ,44       -- 3
--        ,25   ,45       -- 2
--              ,46]      -- 1
--
--         ^^^^             ^^^
--       Elements         Lengths
-- @
--
ratchet :: U.Vector (Int, Int)          -- ^ Starting and ending values.
        -> (U.Vector Int, U.Vector Int) -- ^ Elements and Lengths vectors.
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
{-# INLINE_STREAM ratchet #-}


-------------------------------------------------------------------------------
-- | Extract segments from some source array and concatenate them.
-- 
-- @
--    let arr = [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
--    in  extractS (index arr) [(0, 1), (3, 3), (2, 6)]
--    
--     => [10, 13, 14, 15, 12, 13, 14, 15, 16, 17]
-- @
--
extract :: Unbox a 
        => (Int -> a)           -- ^ Function to get elements from the source.
        -> U.Vector (Int, Int)  -- ^ Segment starts and lengths.
        -> U.Vector a           -- ^ Result elements.

extract get vStartLen
 = G.unstream $ extractS get (G.stream vStartLen)
{-# INLINE_STREAM extract #-}


-------------------------------------------------------------------------------
-- | Segmented fold over vectors of segment lengths and input values.
--
--   The total lengths of all segments need not match the length of the
--   input elements vector. The returned `C.Folds` state can be inspected
--   to determine whether all segments were completely folded, or the 
--   vector of segment lengths or elements was too short relative to the
--   other.
--
foldsT  :: (Unbox a, Unbox b)
        => (a -> b -> b)        -- ^ Worker function.
        -> b                    -- ^ Initial state when folding segments.
        -> Maybe (Int, b)       -- ^ Length and initial state for first segment.
        -> U.Vector Int         -- ^ Segment lengths.
        -> U.Vector a           -- ^ Elements.
        -> (U.Vector b, C.Folds Int Int a b)

foldsT f zN s0 vLens vVals
 = let  
        f' x y = return $ f x y
        {-# INLINE f' #-}

        (vResults, state) 
          = runST $ unchainToVectorM 
                  $ C.foldsC f' zN s0
                        (chainOfVector vLens)
                        (chainOfVector vVals)

   in   (vResults, C.packFolds state)
{-# INLINE_STREAM foldsT #-}

