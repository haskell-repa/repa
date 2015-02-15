{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module Main where
import Data.Repa.Flow
import Data.Repa.Array                  as A
import qualified Data.Repa.Flow.Chunked as C
import qualified Data.Repa.Flow.Generic as G
import Prelude                          as P
import System.Environment
import Data.Char
import Data.Bits

import Data.Repa.Eval.Elt

instance (TargetI l a, Elt a) => Elt (Array l a) where
 zero    = A.fromList name []
 one     = A.fromList name [one]
 touch _ = return ()


main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         [fiKey, fiValue, foShuf]
           -> pShuffle fiKey fiValue foShuf
         _ -> dieUsage


pShuffle fiKey fiValue foShuf
 = do
        -- Read keys and values, and funnel data from all buckets
        -- into single streams.
        iKey     <- G.funnel_i =<< fromDir fiKey   sourceLines
        iVal     <- G.funnel_i =<< fromDir fiValue sourceLines

        -- Shuffle the input data based on the first character.
        iKeyHash <- G.map_i (A.mapS U hash) iKey
        iHashVal <- C.szipWith_ii B (\_ x y -> (x, y)) iKeyHash iVal

        -- We'll output the shuffled result to a new dir.
        oResult  <- toDir   256 foShuf (sinkLines B F)
        oShuf    <- G.dshuffle_o B oResult

        G.drainS iHashVal oShuf


-- TODO: needs a real hash functinon, and don't go via lists.
hash arr
        | A.length arr  == 0  = 0
        | otherwise           
        = sum (P.map (fromIntegral . ord) $ A.toList arr) .&. 0x0ff
{-# INLINE hash #-}


dieUsage
 = error $ P.unlines
 [ "flow-shuffle <in_dir_keys> <in_dir_vals> <out_dir_shuf>" ]
