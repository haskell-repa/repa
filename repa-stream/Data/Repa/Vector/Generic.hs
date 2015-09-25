
-- | Converting `Stream`s and `Chain`s to and from generic `Vector`s.
--
--   * NOTE: Support for streams of unknown length is not complete.
--
module Data.Repa.Vector.Generic
        ( -- * Stream operators
          unstreamToVector2
        , unstreamToMVector2

          -- ** Compacting
        , compact
        , compactIn

          -- ** Dicing
        , findSegments
        , findSegmentsFrom
        , diceSep

          -- ** Extracting
        , extract

          -- ** Inserting
        , insert

          -- ** Merging
        , merge
        , mergeMaybe

          -- ** Padding
        , padForward

          -- ** Ratcheting
        , ratchet

          -- ** Replicating
        , replicates

          -- * Chain operators
        , chainOfVector
        , unchainToVector
        , unchainToMVector

          -- * Folding
        , folds, C.Folds(..)

          -- * Scanning
        , scanMaybe
        , groupsBy)
where
import Data.Repa.Stream.Compact
import Data.Repa.Stream.Concat
import Data.Repa.Stream.Dice
import Data.Repa.Stream.Extract
import Data.Repa.Stream.Insert
import Data.Repa.Stream.Merge
import Data.Repa.Stream.Pad
import Data.Repa.Stream.Ratchet
import Data.Repa.Stream.Replicate
import Data.Repa.Stream.Segment
import Data.Repa.Scalar.Option
import Data.IORef
import System.IO.Unsafe
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Repa.Chain                                  as C
import qualified Data.Vector.Generic                    as GV
import qualified Data.Vector.Generic.Mutable            as GM
import qualified Data.Vector.Fusion.Stream.Monadic      as SM
import qualified Data.Vector.Fusion.Stream.Size         as S
import qualified Data.Vector.Fusion.Stream              as S
#include "repa-stream.h"


-------------------------------------------------------------------------------
-- | Unstream some elements to two separate vectors.
--
--   `Nothing` values are ignored.
--
unstreamToVector2
        :: (PrimMonad m, GV.Vector v a, GV.Vector v b)
        => SM.Stream m (Maybe a, Maybe b)
                                -- ^ Source data.
        -> m (v a, v b)         -- ^ Resulting vectors.

unstreamToVector2 s
 = do   (mvec1, mvec2) <- unstreamToMVector2 s
        vec1    <- GV.unsafeFreeze mvec1
        vec2    <- GV.unsafeFreeze mvec2
        return (vec1, vec2)
{-# INLINE_STREAM unstreamToVector2 #-}


-- | Unstream some elements to two separate mutable vectors.
--
--   `Nothing` values are ignored.
--
unstreamToMVector2
        :: (PrimMonad m, GM.MVector v a, GM.MVector v b)
        => SM.Stream m (Maybe a, Maybe b)                
                                -- ^ Source data.
        -> m (v (PrimState m) a, v (PrimState m) b)     
                                -- ^ Resulting vectors.

unstreamToMVector2 (SM.Stream step s0 sz)
 = case sz of
        S.Exact i       -> unstreamToMVector2_max  i s0 step
        S.Max   i       -> unstreamToMVector2_max  i s0 step
        S.Unknown       -> error "repa-stream: finish unstreamToMVector2"
{-# INLINE_STREAM unstreamToMVector2 #-}

unstreamToMVector2_max nMax s0 step
 =  GM.unsafeNew nMax >>= \vecL
 -> GM.unsafeNew nMax >>= \vecR
 -> let 
        go !sPEC !iL !iR !s
         =  step s >>= \m
         -> case m of
                SM.Yield (mL, mR) s'
                 -> do  !iL' <- case mL of
                                Nothing -> return iL
                                Just xL -> do GM.unsafeWrite vecL iL xL
                                              return (iL + 1)

                        !iR' <- case mR of
                                Nothing -> return iR
                                Just xR -> do GM.unsafeWrite vecR iR xR
                                              return (iR + 1)

                        go sPEC iL' iR' s'
                       
                SM.Skip s' 
                 ->     go sPEC iL iR s'

                SM.Done
                 ->     return  ( GM.unsafeSlice 0 iL vecL
                                , GM.unsafeSlice 0 iR vecR)
    in go SM.SPEC 0 0 s0
{-# INLINE_STREAM unstreamToMVector2_max #-}


-------------------------------------------------------------------------------
-- | Combination of `fold` and `filter`. 
--   
--   We walk over the stream front to back, maintaining an accumulator.
--   At each point we can chose to emit an element (or not)
--
compact :: (GV.Vector v a, GV.Vector v b)
        => (s -> a -> (s, Maybe b))     -- ^ Worker function
        -> s                            -- ^ Starting state
        -> v a                          -- ^ Input vector
        -> v b

compact f s0 vec
        = GV.unstream $ compactS f s0 $ GV.stream vec
{-# INLINE compact #-}


-- | Like `compact` but use the first value of the stream as the 
--   initial state, and add the final state to the end of the output.
compactIn
        :: GV.Vector v a
        => (a -> a -> (a, Maybe a))     -- ^ Worker function.
        -> v a                          -- ^ Input elements.
        -> v a

compactIn f vec
        = GV.unstream $ compactInS f $ GV.stream vec
{-# INLINE compactIn #-}


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
        :: (GV.Vector v a, GV.Vector v Int, GV.Vector v (Int, Int))
        => (a -> Bool)          -- ^ Predicate to check for start of segment.
        -> (a -> Bool)          -- ^ Predicate to check for end of segment.
        ->  v a                 -- ^ Input vector.
        -> (v Int, v Int)

findSegments pStart pEnd src
        = GV.unzip
        $ GV.unstream
        $ startLengthsOfSegsS
        $ findSegmentsS pStart pEnd (GV.length src - 1)
        $ SM.indexed 
        $ GV.stream src
{-# INLINE findSegments #-}


-------------------------------------------------------------------------------
-- | Given predicates that detect the beginning and end of some interesting
--   segment of information, scan through a vector looking for when these
--   segments begin and end. Return vectors of the segment starting positions
--   and lengths.
findSegmentsFrom
        :: (GV.Vector v Int, GV.Vector v (Int, Int))
        => (a -> Bool)          -- ^ Predicate to check for start of segment.
        -> (a -> Bool)          -- ^ Predicate to check for end of segment.
        -> Int                  -- ^ Input length.
        -> (Int -> a)           -- ^ Get an element from the input.
        -> (v Int, v Int)

findSegmentsFrom pStart pEnd len get
        = GV.unzip
        $ GV.unstream
        $ startLengthsOfSegsS
        $ findSegmentsS pStart pEnd (len - 1)
        $ SM.map         (\ix -> (ix, get ix))
        $ SM.enumFromStepN 0 1 len
{-# INLINE findSegmentsFrom #-}


-------------------------------------------------------------------------------
-- | Dice a vector stream into rows and columns.
--
diceSep :: (GV.Vector v a, GV.Vector v (Int, Int))
        => (a -> Bool)                  -- ^ Detect the end of a column.
        -> (a -> Bool)                  -- ^ Detect the end of a row.
        ->  v a
        -> (v (Int, Int), v (Int, Int)) -- ^ Segment starts   and lengths

diceSep pEndInner pEndBoth vec
        = runST
        $ unstreamToVector2
        $ diceSepS pEndInner pEndBoth 
        $ S.liftStream
        $ GV.stream vec
{-# INLINE diceSep #-}


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
extract :: (GV.Vector v (Int, Int), GV.Vector v a)
        => (Int -> a)           -- ^ Function to get elements from the source.
        -> v (Int, Int)  -- ^ Segment starts and lengths.
        -> v a           -- ^ Result elements.

extract get vStartLen
 = GV.unstream $ extractS get $ GV.stream vStartLen
{-# INLINE extract #-}


-------------------------------------------------------------------------------
-- | Insert elements produced by the given function into a vector.
insert  :: GV.Vector v a
        => (Int -> Maybe a)     -- ^ Produce a new element for this index.
        -> v a                  -- ^ Source vector.
        -> v a

insert fNew vec
 = GV.unstream $ insertS fNew $ GV.stream vec
{-# INLINE insert #-}


-------------------------------------------------------------------------------
-- | Merge two pre-sorted key-value streams.
merge   :: ( Ord k
           , GV.Vector v k
           , GV.Vector v (k, a)
           , GV.Vector v (k, b)
           , GV.Vector v (k, c))
        => (k -> a -> b -> c)   -- ^ Combine two values with the same key.
        -> (k -> a -> c)        -- ^ Handle a left value without a right value.
        -> (k -> b -> c)        -- ^ Handle a right value without a left value.
        -> v (k, a)             -- ^ Vector of keys and left values.
        -> v (k, b)             -- ^ Vector of keys and right values.
        -> v (k, c)             -- ^ Vector of keys and results.

merge fBoth fLeft fRight vA vB
        = GV.unstream 
        $ mergeS fBoth fLeft fRight 
                (GV.stream vA) 
                (GV.stream vB)
{-# INLINE merge #-}


-- | Like `merge`, but only produce the elements where the worker functions
--   return `Just`.
mergeMaybe 
        :: ( Ord k
           , GV.Vector v k
           , GV.Vector v (k, a)
           , GV.Vector v (k, b)
           , GV.Vector v (k, c))
        => (k -> a -> b -> Maybe c) -- ^ Combine two values with the same key.
        -> (k -> a -> Maybe c)      -- ^ Handle a left value without a right value.
        -> (k -> b -> Maybe c)      -- ^ Handle a right value without a left value.
        -> v (k, a)                 -- ^ Vector of keys and left values.
        -> v (k, b)                 -- ^ Vector of keys and right values.
        -> v (k, c)                 -- ^ Vector of keys and results.

mergeMaybe fBoth fLeft fRight vA vB
        = GV.unstream
        $ catMaybesS
        $ SM.map  munge_mergeMaybe
        $ mergeS fBoth fLeft fRight
                (GV.stream vA)
                (GV.stream vB)

        where   munge_mergeMaybe (_k, Nothing)   = Nothing
                munge_mergeMaybe (k,  Just x)    = Just (k, x)
                {-# INLINE munge_mergeMaybe #-}
{-# INLINE mergeMaybe #-}


-------------------------------------------------------------------------------
-- | Given a stream of keys and values, and a successor function for keys, 
--   if the stream is has keys missing in the sequence then insert 
--   the missing key, copying forward the the previous value.
padForward  
        :: (Ord k, GV.Vector v (k, a))
        => (k -> k)             -- ^ Successor function.
        -> v (k, a)             -- ^ Input keys and values.
        -> v (k, a)

padForward ksucc vec
        = GV.unstream
        $ padForwardS ksucc
        $ GV.stream vec
{-# INLINE padForward #-}


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
ratchet :: (GV.Vector v Int, GV.Vector v (Int, Int))
        => v (Int, Int)         -- ^ Starting and ending values.
        -> (v Int, v Int)       -- ^ Elements and Lengths vectors.
ratchet vStartsMax 
 = unsafePerformIO
 $ do   
        -- Make buffers for the start values and unpack the max values.
        let (vStarts, vMax) = GV.unzip vStartsMax
        mvStarts   <- GV.thaw vStarts

        -- Make a vector for the output lengths.
        mvLens     <- GM.unsafeNew (GV.length vStartsMax)
        rmvLens    <- newIORef mvLens

        -- Run the computation
        mvStarts'  <- GM.munstream $ unsafeRatchetS mvStarts vMax rmvLens

        -- Read back the output segment lengths and freeze everything.
        mvLens'    <- readIORef rmvLens
        vStarts'   <- GV.unsafeFreeze mvStarts'
        vLens'     <- GV.unsafeFreeze mvLens'
        return (vStarts', vLens')
{-# INLINE ratchet #-}


-------------------------------------------------------------------------------
-- | Segmented replicate.
replicates
        :: (GV.Vector v (Int, a), GV.Vector v a)
        => v (Int, a)
        -> v a

replicates vec
        = GV.unstream
        $ replicatesS
        $ GV.stream vec
{-# INLINE replicates #-}


-------------------------------------------------------------------------------
-- | Produce a chain from a generic vector.
chainOfVector 
        :: (Monad m, GV.Vector v a)
        => v a -> Chain m Int a

chainOfVector vec
 = Chain (S.Exact len) 0 step
 where
        !len  = GV.length vec

        step !i
         | i >= len
         = return $ Done  i

         | otherwise    
         = return $ Yield (GV.unsafeIndex vec i) (i + 1)
        {-# INLINE_INNER step #-}
{-# INLINE_STREAM chainOfVector #-}


-------------------------------------------------------------------------------
-- | Compute a chain into a generic vector.
unchainToVector
        :: (PrimMonad m, GV.Vector v a)
        => C.Chain m s a  -> m (v a, s)
unchainToVector chain
 = do   (mvec, c') <- unchainToMVector chain
        vec        <- GV.unsafeFreeze mvec
        return (vec, c')
{-# INLINE_STREAM unchainToVector #-}


-- | Compute a chain into a generic mutable vector.
unchainToMVector
        :: (PrimMonad m, GM.MVector v a)
        => Chain m s a
        -> m (v (PrimState m) a, s)

unchainToMVector (Chain sz s0 step)
 = case sz of
        S.Exact i       -> unchainToMVector_max     i  s0 step
        S.Max i         -> unchainToMVector_max     i  s0 step
        S.Unknown       -> unchainToMVector_unknown 32 s0 step
{-# INLINE_STREAM unchainToMVector #-}


-- unchain when we known the maximum size of the vector.
unchainToMVector_max nMax s0 step 
 =  GM.unsafeNew nMax >>= \vec
 -> let 
        go !sPEC !i !s
         =  step s >>= \m
         -> case m of
                Yield e s'
                 -> do  GM.unsafeWrite vec i e
                        go sPEC (i + 1) s'

                Skip s' -> go sPEC i s'
                Done s' -> return (GM.unsafeSlice 0 i vec, s')
        {-# INLINE_INNER go #-}

    in  go SM.SPEC 0 s0
{-# INLINE_STREAM unchainToMVector_max #-}


-- unchain when we don't know the maximum size of the vector.
unchainToMVector_unknown nStart s0 step
 =  GM.unsafeNew nStart >>= \vec0
 -> let 
        go !sPEC !vec !i !n !s
         =  step s >>= \m
         -> case m of
                Yield e s'
                 | i >= n       
                 -> do  vec'    <- GM.unsafeGrow vec n
                        GM.unsafeWrite vec' i e
                        go sPEC vec' (i + 1) (n + n) s'

                 | otherwise
                 -> do  GM.unsafeWrite vec i e
                        go sPEC vec  (i + 1) n s'

                Skip s' -> go sPEC vec i n s'
                Done s' -> return (GM.unsafeSlice 0 i vec, s')
        {-# INLINE_INNER go #-}

    in go SM.SPEC vec0 0 nStart s0
{-# INLINE_STREAM unchainToMVector_unknown #-}


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
folds   :: forall v n a b
        .  (GV.Vector v (n, Int), GV.Vector v a, GV.Vector v (n, b))
        => (a -> b -> b)        -- ^ Worker function to fold each segment.
        -> b                    -- ^ Initial state when folding segments.
        -> Option3 n Int b      -- ^ Length and initial state for first segment.
        -> v (n, Int)           -- ^ Segment names and lengths.
        -> v a                  -- ^ Elements.
        -> (v (n, b), C.Folds Int Int n a b)

folds f zN s0 vLens vVals
 = let  
        f' x y = return $ f x y
        {-# INLINE f' #-}

        (vResults :: v (n, b), state) 
          = runST $ unchainToVector
                  $ C.foldsC f' zN s0
                        (chainOfVector vLens)
                        (chainOfVector vVals)

   in   (vResults, state)
{-# INLINE folds #-}


-------------------------------------------------------------------------------
-- | Perform a left-to-right scan through an input vector, maintaining a state
--   value between each element. For each element of input we may or may not
--   produce an element of output.
scanMaybe 
        :: forall v1 v2 a b s
        .  (GV.Vector v1 a, GV.Vector v2 b)
        =>  (s -> a -> (s, Maybe b))    -- ^ Worker function.
        ->  s                           -- ^ Initial state for scan.
        ->  v1 a                        -- ^ Input elements.
        -> (v2 b, s)                    -- ^ Output elements.

scanMaybe f k0 vec0
 = (vec1, snd k1)
 where  
        f' s x = return $ f s x

        (vec1 :: v2 b, k1 :: (Int, s))
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
        :: forall v1 v2 a
        .  (GV.Vector v1 a, GV.Vector v2 (a, Int))
        => (a -> a -> Bool)             -- ^ Comparison function.
        -> Maybe (a, Int)               -- ^ Starting element and count.
        ->  v1 a                         -- ^ Input elements.
        -> (v2 (a, Int), Maybe (a, Int))

groupsBy f !c !vec0
 = (vec1, snd k1)
 where  
        f' x y = return $ f x y

        (vec1 :: v2 (a, Int), k1)
         = runST $ unchainToVector   $ C.liftChain 
                 $ C.groupsByC f' c  $ chainOfVector vec0
{-# INLINE groupsBy #-}

