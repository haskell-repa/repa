
module Data.Repa.Array.Internals.Operator.Fold
        (folds, C.Folds(..), FoldsDict)
where
import Data.Repa.Array.Index                    as A
import Data.Repa.Array.Tuple                    as A
import Data.Repa.Array.Internals.Bulk           as A
import Data.Repa.Array.Internals.Target         as A
import Data.Repa.Eval.Chain                     as A
import Data.Repa.Fusion.Unpack                  as A
import qualified Data.Repa.Chain                as C
import Data.Repa.Fusion.Option
import System.IO.Unsafe
#include "repa-stream.h"


-- | Segmented fold over vectors of segment lengths and input values.
--
--   * The total lengths of all segments need not match the length of the
--     input elements vector. The returned `C.Folds` state can be inspected
--     to determine whether all segments were completely folded, or the 
--     vector of segment lengths or elements was too short relative to the
--     other.
--
folds   :: FoldsDict lSeg lElt lGrp tGrp lRes tRes n a b
        => lGrp                  -- ^ Layout for group names.
        -> lRes                  -- ^ Layout for fold results.
        -> (a -> b -> b)         -- ^ Worker function.
        -> b                     -- ^ Initial state when folding segments.
        -> Option3 n Int b       -- ^ Length and initial state for first segment.
        ->  Array lSeg (n, Int)  -- ^ Segment names and lengths.
        ->  Array lElt a         -- ^ Elements.
        -> (Array (T2 lGrp lRes) (n, b), C.Folds Int Int n a b)

folds lGrp lRes f z s0 vLens vVals
 = unsafePerformIO
 $ do   
        let f' !x !y = return $ f x y
            {-# INLINE f' #-}

        A.unchainToArrayIO (T2 lGrp lRes)
         $  C.foldsC f' z s0
                (A.chainOfArray vLens)
                (A.chainOfArray vVals)
{-# INLINE_ARRAY folds #-}


-- | Dictionaries need to perform a segmented fold.
type FoldsDict lSeg lElt lGrp tGrp lRes tRes n a b
      = ( Bulk   lSeg (n, Int)
        , Bulk   lElt a
        , Target lGrp n 
        , Target lRes b
        , Index  lGrp ~ Index lRes
        , Unpack (Buffer lGrp n) tGrp
        , Unpack (Buffer lRes b) tRes)
