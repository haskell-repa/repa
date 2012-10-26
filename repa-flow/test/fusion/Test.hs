
module Test where
import Data.Array.Repa.Flow             as F
import qualified Data.Vector.Unboxed    as U



testFlowUnflow :: U.Vector Int -> IO (U.Vector Int)
testFlowUnflow vec
 = do   f       <- flow vec
        unflow f



testMap :: U.Vector Int -> IO (U.Vector Int)
testMap vec
 = do   f       <- flow vec
        unflow  $ F.map (+ 1234) f


testMapMap :: U.Vector Int -> IO (U.Vector Int)
testMapMap vec
 = do   f       <- flow vec
        unflow  $ F.map (+ 1234) $ F.map (* 3456) $ f
