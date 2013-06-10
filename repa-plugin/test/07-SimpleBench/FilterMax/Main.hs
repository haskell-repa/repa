{-# LANGUAGE BangPatterns #-}
module Main where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.IO.Timing        as R
import qualified Data.Vector.Unboxed    as U
import GHC.Exts
import System.Environment


---------------------------------------------------------------------
-- | Set the primitives used by the lowering transform.
repa_primitives :: R.Primitives
repa_primitives =  R.primitives


---------------------------------------------------------------------
main
 = do   args    <- getArgs
        let (alg, sz)  
                 = case args of
                     [alg', szStr] -> (alg', Prelude.read szStr :: Int)
                     _             -> error "Usage: simplebench <alg> <size>"

        let !uvec = U.enumFromN (0 :: Int) sz

        ((uvec', n1, n2), t) 
         <- R.time $ do (uvec', n1, n2) <- run alg uvec
                        uvec' `seq` n1 `seq` n2 `seq` return (uvec', n1, n2)

        print (n1, n2)
        putStr (prettyTime t)

run "vector" uvec = run_vector uvec
run "flow"   uvec = run_flow   uvec
        

run_vector :: U.Vector Int  -> IO (U.Vector Int, Int, Int)
run_vector uvec1
 = let  uvec2   = U.map (+ 1) uvec1
        uvec4   = U.filter (\x -> x > 0) uvec2
        n1      = U.foldl maxx 1 uvec4
        n2      = U.foldl minx 0 uvec4
   in   return  (uvec4, n1, n2)
{-# NOINLINE run_vector #-}


run_flow  :: U.Vector Int -> IO (U.Vector Int, Int, Int)
run_flow uvec
 = do   vvec2   <- V.fromUnboxed uvec
        let (vvec3, n1, n2) = R.runSeries vvec2 lower_maxx
        uvec3   <- V.toUnboxed vvec3
        return  (uvec3, n1, n2)
{-# NOINLINE run_flow #-}



-- | Get the vector of positive values,
--   as well as the maximual element.
lower_maxx :: Series k Int -> (Vector Int, Int, Int)
lower_maxx s1
 = let  s2      = R.map (\x -> x + 1) s1
        s3      = R.map (\x -> x > 0) s2
   in   R.mkSel1 s3 
         (\sel -> let s4   = R.pack sel s2
                  in  ( S.toVector s4
                      , R.fold maxx 1 s4
                      , R.fold minx 0 s4))

maxx :: Int -> Int -> Int
maxx x y
 = if x > y then x else y
{-# INLINE [0] maxx #-}

minx :: Int -> Int -> Int
minx x y
 = if x > y then y else x
{-# INLINE [0] minx #-}
