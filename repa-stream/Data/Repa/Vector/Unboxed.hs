{-# LANGUAGE CPP #-}
module Data.Repa.Vector.Unboxed
        ( -- * Conversion
          chainOfVector
        , unchainToVector
        , unchainToMVector

        , findSegments
        , findSegmentsFrom
        , ratchet
        , extract

          -- * Scan operators
          -- | These have a scan-like structure,
          --   and are implemented in terms of `scanMaybe`.
        , scanMaybe

          -- ** Grouping
        , groupsBy

          -- * Weave operators
          -- | These have a weave-like structure,
          --   and are implemented in terms of `weave`.

          -- ** Folding
        , folds, C.Folds(..))
where
import Data.Repa.Fusion.Option
import Data.Repa.Stream.Extract
import Data.Repa.Stream.Ratchet
import Data.Repa.Stream.Segment
import Data.Vector.Unboxed                              (Unbox, Vector)
import Data.Vector.Unboxed.Mutable                      (MVector)
import Data.Repa.Chain                                  (Chain)
import qualified Data.Repa.Vector.Generic               as G
import qualified Data.Repa.Chain                        as C
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
-- | Produce a chain from a generic vector.
chainOfVector 
        :: (Monad m, Unbox a)
        => Vector a -> Chain m Int a
chainOfVector = G.chainOfVector
{-# INLINE_STREAM chainOfVector #-}


-- | Compute a chain into a vector.
unchainToVector
        :: (PrimMonad m, Unbox a)
        => C.Chain m s a  -> m (Vector a, s)
unchainToVector = G.unchainToVector
{-# INLINE_STREAM unchainToVector #-}


-- | Compute a chain into a mutable vector.
unchainToMVector
        :: (PrimMonad m, Unbox a)
        => C.Chain m s a
        -> m (MVector (PrimState m) a, s)
unchainToMVector = G.unchainToMVector
{-# INLINE_STREAM unchainToMVector #-}


-------------------------------------------------------------------------------
-- | Perform a left-to-right scan through an input vector, maintaining a state
--   value between each element. For each element of input we may or may not
--   produce an element of output.
scanMaybe 
        :: (Unbox a, Unbox b)
        => (s -> a -> (s, Maybe b))     -- ^ Worker function.
        ->  s                           -- ^ Initial state for scan.
        ->  U.Vector a                  -- ^ Input elements.
        -> (U.Vector b, s)              -- ^ Output elements.

scanMaybe f k0 vec0
 = (vec1, snd k1)
 where  
        f' s x = return $ f s x

        (vec1, k1)
         = runST $ unchainToVector     $ C.liftChain 
                 $ C.scanMaybeC f' k0  $ chainOfVector vec0
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
         = runST $ unchainToVector   $ C.liftChain 
                 $ C.groupsByC f' c  $ chainOfVector vec0
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
--   other. In the resulting state, `C.foldLensState` is the index into
--   the lengths vector *after* the last one that was consumed. If this
--   equals the length of the lengths vector then all segment lengths were
--   consumed. Similarly for the elements vector.
--
folds   :: (Unbox a, Unbox b)
        => (a -> b -> b)        -- ^ Worker function to fold each segment.
        -> b                    -- ^ Initial state when folding segments.
        -> Option2 Int b        -- ^ Length and initial state for first segment.
        -> U.Vector Int         -- ^ Segment lengths.
        -> U.Vector a           -- ^ Elements.
        -> (U.Vector b, C.Folds Int Int a b)

folds f zN s0 vLens vVals
 = let  
        f' x y = return $ f x y
        {-# INLINE f' #-}

        (vResults, state) 
          = runST $ unchainToVector
                  $ C.foldsC f' zN s0
                        (chainOfVector vLens)
                        (chainOfVector vVals)

   in   (vResults, state)
{-# INLINE_STREAM folds #-}

