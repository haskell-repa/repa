import DPH.Testsuite
import DPH.Arbitrary.Segd
import DPH.Arbitrary.Int
import Data.Array.Parallel.Unlifted as U
import Prelude as P

import Data.Vector.Repa.Operators.Chains
import qualified Data.Array.Repa	as R
import qualified Data.Array.Repa.Eval	as RE
import qualified Data.Vector.Repa     	as RV
import qualified Data.Vector.Repa.Repr.Chained     	as RC

repaOfList :: (RE.Target R.U a) => [a] -> RV.Vector R.U a
repaOfList xs = RE.fromList (R.ix1 $ P.length xs) xs

listOfChain :: (RE.Target R.U a, Elt a) => RV.Vector RC.N a -> [a]
listOfChain x = R.toList $ repaOfChain x

repaOfChain :: (RE.Target R.U a) => RV.Vector RC.N a -> RV.Vector R.U a
repaOfChain x = RV.vunchainP x

$(testcases [ ""        <@ [t| ( Bool, Int ) |]
            ]
  [d|
    -- replicate each element such that the resulting array such corresponds to
    -- the given segment descriptor e.g.:
    -- replicate_s [1,3,1] [1,2,3] = [1,2,2,2,3]
    prop_replicate_s :: (Eq a, RE.Target R.U a, Elt a) => Array Int -> Array a -> Bool
    prop_replicate_s lens arr =
      listOfChain (replicates segd (repaOfList $ toList arr))
      == P.concat (P.zipWith P.replicate (toList lens') (toList arr'))
      where segd = mkSegd lens' (U.scan (+) 0 lens') (U.sum lens')
            -- make the arrays equal length
            (lens', arr') = U.unzip $ U.zip (U.map (`mod` maxRepl) lens) arr
            maxRepl = 100

    -- interleaves the segments of two arrays, e.g.:
    -- append_s (Segd [1,1,2,1]) (Segd [1,2]) [1,3,4] (Segd [1,1]) [2,5]
    --   = [1,2,3,4,5]
    prop_append_s :: (Eq a, RE.Target R.U a, Elt a) => Array a -> Array a -> Property
    prop_append_s arr brr =
      forAll (segdForArray arr) $ \segd1 ->
      forAll (segdForArray brr) $ \segd2 ->
      let lens1  = lengthsSegd segd1
          lens2  = lengthsSegd segd2
          lens1' = toList lens1
          lens2' = toList lens2
          arr'   = toList arr
          brr'   = toList brr
      in 
         P.length lens1' == P.length lens2' ==>
         listOfChain (appendSUP (segdFrom lens1 lens2) segd1 (repaOfList $ toList arr) segd2 (repaOfList $ toList brr)) ==
         concat (interleave (nest lens1' arr') (nest lens2' brr'))
      where segdFrom lens1 lens2 = lengthsToSegd $ U.interleave lens1 lens2
            lengthsToSegd lens = mkSegd lens (scan (+) 0 lens) (U.sum lens)
--            interleave :: [a] -> [a] -> [a]
            interleave (x : xs) (y : ys) = x : y : interleave xs ys
            interleave (x : xs) _        = [x]
            interleave _        _        = []
  |])

