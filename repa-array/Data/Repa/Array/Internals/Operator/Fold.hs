
-- | Folding operations on arrays.
module Data.Repa.Array.Internals.Operator.Fold
        ( -- * Segmented fold
          folds
        , foldsWith
        , C.Folds(..), FoldsDict)
where
import Data.Repa.Array.Generic.Index            as A
import Data.Repa.Array.Meta.Tuple               as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Eval.Chain                     as A
import Data.Repa.Fusion.Unpack                  as A
import Data.Repa.Scalar.Option
import System.IO.Unsafe
import qualified Data.Repa.Chain                as C
import Prelude hiding (foldl)
#include "repa-array.h"


-- | Segmented fold over vectors of segment lengths and input values.
--
--   * The total lengths of all segments need not match the length of the
--     input elements vector. The returned `C.Folds` state can be inspected
--     to determine whether all segments were completely folded, or the
--     vector of segment lengths or elements was too short relative to the
--     other.
--
-- @
-- > import Data.Repa.Array.Material
-- > import Data.Repa.Nice
-- > let segs  = fromList B [("red", 3), ("green", 5)]
-- > let vals  = fromList U [0..100 :: Int]
-- > nice $ fst $ folds B U (+) 0 segs vals
-- [("red",3),("green",25)]
-- @
--
folds   :: FoldsDict lSeg lElt lGrp tGrp lRes tRes n a b
        => Name lGrp            -- ^ Layout for group names.
        -> Name lRes            -- ^ Layout for fold results.
        -> (a -> b -> b)        -- ^ Worker function.
        -> b                    -- ^ Initial state when folding segments.
        -> Array lSeg (n, Int)   -- ^ Segment names and lengths.
        -> Array lElt a          -- ^ Elements.
        -> (Array (T2 lGrp lRes) (n, b), C.Folds Int Int n a b)

folds nGrp nRes f z vLens vVals
        = foldsWith nGrp nRes f z Nothing vLens vVals
{-# INLINE_ARRAY folds #-}


-- | Like `folds`, but take an initial state for the first segment.
--
-- @
-- > import Data.Repa.Array.Material
-- > import Data.Repa.Nice
-- > let state = Just ("white", 4, 100)
-- > let segs  = fromList B [("red", 3), ("green", 5)]
-- > let vals  = fromList U [0..100 :: Int]
-- > nice $ fst $ foldsWith B U (+) 0  state segs vals
-- [("white",106),("red",15),("green",45)]
-- @
--
foldsWith
        :: FoldsDict lSeg lElt lGrp tGrp lRes tRes n a b
        => Name lGrp             -- ^ Layout for group names.
        -> Name lRes             -- ^ Layout for fold results.
        -> (a -> b -> b)         -- ^ Worker function.
        -> b                     -- ^ Initial state when folding segments.
        -> Maybe (n, Int, b)     -- ^ Name, length and initial state for first segment.
        -> Array lSeg (n, Int)   -- ^ Segment names and lengths.
        -> Array lElt a          -- ^ Elements.
        -> (Array (T2 lGrp lRes) (n, b), C.Folds Int Int n a b)

foldsWith nGrp nRes f z s0 vLens vVals
 = unsafePerformIO
 $ do
        let f' !x !y = return $ f x y
            {-# INLINE f' #-}

        let !s0'     = case s0 of
                        Nothing           -> None3
                        Just (a1, a2, a3) -> Some3 a1 a2 a3

        A.unchainToArrayIO (T2 nGrp nRes)
         $  C.foldsC f' z s0'
                (A.chainOfArray vLens)
                (A.chainOfArray vVals)
{-# INLINE_ARRAY foldsWith #-}


-- | Dictionaries need to perform a segmented fold.
type FoldsDict lSeg lElt lGrp tGrp lRes tRes n a b
      = ( Bulk   lSeg (n, Int)
        , Bulk   lElt a
        , Target lGrp n
        , Target lRes b
        , Index  lGrp ~ Index lRes
        , Unpack (Buffer lGrp n) tGrp
        , Unpack (Buffer lRes b) tRes)
