
module Data.Repa.Vector.Unboxed
        ( -- * Conversion
          chainOfVector
        , unchainToVector
        , unchainToMVector

          -- * Generating
        , ratchet

          -- * Extracting
        , extract

          -- * Inserting
        , insert

          -- * Merging
        , merge
        , mergeMaybe

          -- * Splitting
        , findSegments
        , findSegmentsFrom
        , diceSep

          -- * Padding
        , padForward

          -- * Scanning
        , scanMaybe

          -- * Grouping
        , groupsBy

          -- * Folding
        , folds, C.Folds(..))
where
import Data.Repa.Option
import Data.Repa.Stream.Concat
import Data.Repa.Stream.Dice
import Data.Repa.Stream.Extract
import Data.Repa.Stream.Insert
import Data.Repa.Stream.Merge
import Data.Repa.Stream.Pad
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
import qualified Data.Vector.Fusion.Stream              as S
import Control.Monad.ST
import Control.Monad.Primitive
import System.IO.Unsafe
import Data.IORef
#include "repa-stream.h"


-------------------------------------------------------------------------------
-- | Produce a chain from a generic vector.
chainOfVector 
        :: (Monad m, Unbox a)
        => Vector a -> Chain m Int a
chainOfVector = G.chainOfVector
{-# INLINE chainOfVector #-}


-- | Compute a chain into a vector.
unchainToVector
        :: (PrimMonad m, Unbox a)
        => C.Chain m s a  -> m (Vector a, s)
unchainToVector = G.unchainToVector
{-# INLINE unchainToVector #-}


-- | Compute a chain into a mutable vector.
unchainToMVector
        :: (PrimMonad m, Unbox a)
        => C.Chain m s a
        -> m (MVector (PrimState m) a, s)
unchainToMVector = G.unchainToMVector
{-# INLINE unchainToMVector #-}



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
        mvLens     <- UM.unsafeNew (U.length vStartsMax)
        rmvLens    <- newIORef mvLens

        -- Run the computation
        mvStarts'  <- GM.munstream $ unsafeRatchetS mvStarts vMax rmvLens

        -- Read back the output segment lengths and freeze everything.
        mvLens'    <- readIORef rmvLens
        vStarts'   <- G.unsafeFreeze mvStarts'
        vLens'     <- G.unsafeFreeze mvLens'
        return (vStarts', vLens')
{-# INLINE ratchet #-}


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
 = G.unstream $ extractS get $ G.stream vStartLen
{-# INLINE extract #-}


-------------------------------------------------------------------------------
-- | Insert elements produced by the given function into a vector.
insert   :: Unbox a
        => (Int -> Maybe a)     -- ^ Produce a new element for this index.
        -> U.Vector a           -- ^ Source vector.
        -> U.Vector a

insert fNew vec
 = G.unstream $ insertS fNew $ G.stream vec
{-# INLINE insert #-}


-------------------------------------------------------------------------------
-- | Merge two pre-sorted key-value streams.
merge   :: (Ord k, Unbox k, Unbox a, Unbox b, Unbox c)
        => (k -> a -> b -> c)   -- ^ Combine two values with the same key.
        -> (k -> a -> c)        -- ^ Handle a left value without a right value.
        -> (k -> b -> c)        -- ^ Handle a right value without a left value.
        -> U.Vector (k, a)      -- ^ Vector of keys and left values.
        -> U.Vector (k, b)      -- ^ Vector of keys and right values.
        -> U.Vector (k, c)      -- ^ Vector of keys and results.

merge fBoth fLeft fRight vA vB
        = G.unstream 
        $ mergeS fBoth fLeft fRight 
                (G.stream vA) 
                (G.stream vB)
{-# INLINE merge #-}


-- | Like `merge`, but only produce the elements where the worker functions
--   return `Just`.
mergeMaybe 
        :: (Ord k, Unbox k, Unbox a, Unbox b, Unbox c)
        => (k -> a -> b -> Maybe c) -- ^ Combine two values with the same key.
        -> (k -> a -> Maybe c)      -- ^ Handle a left value without a right value.
        -> (k -> b -> Maybe c)      -- ^ Handle a right value without a left value.
        -> U.Vector (k, a)          -- ^ Vector of keys and left values.
        -> U.Vector (k, b)          -- ^ Vector of keys and right values.
        -> U.Vector (k, c)          -- ^ Vector of keys and results.

mergeMaybe fBoth fLeft fRight vA vB
        = G.unstream
        $ catMaybesS
        $ S.map  munge_mergeMaybe
        $ mergeS fBoth fLeft fRight
                (G.stream vA)
                (G.stream vB)

        where   munge_mergeMaybe (_k, Nothing)   = Nothing
                munge_mergeMaybe (k,  Just x)    = Just (k, x)
                {-# INLINE munge_mergeMaybe #-}

{-# INLINE mergeMaybe #-}


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
{-# INLINE scanMaybe #-}


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
{-# INLINE groupsBy #-}


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
{-# INLINE findSegments #-}


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
{-# INLINE findSegmentsFrom #-}


-------------------------------------------------------------------------------
-- | Dice a vector stream into rows and columns.
--
diceSep :: Unbox a
        => (a -> Bool)  -- ^ Detect the end of a column.
        -> (a -> Bool)  -- ^ Detect the end of a row.
        -> U.Vector a
        -> (U.Vector (Int, Int), U.Vector (Int, Int))
                        -- ^ Segment starts   and lengths

diceSep pEndInner pEndBoth vec
        = runST
        $ G.unstreamToVector2
        $ diceSepS pEndInner pEndBoth 
        $ S.liftStream
        $ G.stream vec
{-# INLINE diceSep #-}


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
folds   :: (Unbox n, Unbox a, Unbox b)
        => (a -> b -> b)        -- ^ Worker function to fold each segment.
        -> b                    -- ^ Initial state when folding segments.
        -> Option3 n Int b      -- ^ Length and initial state for first segment.
        -> U.Vector (n, Int)    -- ^ Segment names and lengths.
        -> U.Vector a           -- ^ Elements.
        -> (U.Vector (n, b), C.Folds Int Int n a b)

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
{-# INLINE folds #-}


-------------------------------------------------------------------------------
-- | Given a stream of keys and values, and a successor function for keys, 
--   if the stream is has keys missing in the sequence then insert 
--   the missing key, copying forward the the previous value.
padForward  
        :: (Unbox k, Unbox v, Ord k)
        => (k -> k)             -- ^ Successor function.
        -> U.Vector (k, v)      -- ^ Input keys and values.
        -> U.Vector (k, v)

padForward ksucc vec
        = G.unstream
        $ padForwardS ksucc
        $ G.stream vec
{-# INLINE padForward #-}

