{-# LANGUAGE BangPatterns #-}
module Main where
import Data.Array.Repa.Series                   as R
import Data.Array.Repa.Series.Series            as S
import Data.Array.Repa.Series.Vector            as V
import Data.Array.Repa.IO.Timing                as R
import Data.Array.Repa.Series.Ref               (Ref)
import qualified Data.Array.Repa.Series.Ref     as Ref
import qualified Data.Vector.Primitive          as P
import GHC.Exts
import System.Environment


-------------------------------------------------------------------------------
-- | Set the primitives used by the lowering transform.
repa_primitives :: R.Primitives
repa_primitives =  R.primitives


-------------------------------------------------------------------------------
main :: IO ()
main
 = do   args    <- getArgs
        let (alg, sz)  
                 = case args of
                     [alg', szStr] -> (alg', Prelude.read szStr :: Int)
                     _             -> error "Usage: filterMax <alg> <size>"

        let !vec = P.enumFromN (0 :: Int) sz

        ((vec', n1), t) 
         <- R.time $ do (vec', n1) <- run alg vec
                        vec' `seq` n1 `seq` return (vec', n1)

        print n1
        putStr (prettyTime t)

run "vector" vec = run_vector vec
run "flow"   vec = run_flow   vec
        

-------------------------------------------------------------------------------
run_vector :: P.Vector Int  -> IO (P.Vector Int, Int)
run_vector vec1
 = let  vec2   = P.map (+ 1) vec1
        vec4   = P.filter (\x -> x > 0) vec2
        n1     = P.foldl maxx 0 vec4
   in   return  (vec4, n1)
{-# NOINLINE run_vector #-}


run_flow  :: P.Vector Int -> IO (P.Vector Int, Int)
run_flow pvec
 = do   vec     <- V.fromPrimitive pvec
        vec'    <- V.new (P.length pvec)
        rMax    <- Ref.new 0 
        R.runProcess vec (filterMax vec' rMax)
        mx      <- Ref.read rMax
        pvec'   <- V.toPrimitive vec'
        return  (pvec', mx)
{-# NOINLINE run_flow #-}


-- | Get the vector of positive values,
--   as well as the maximual element.
filterMax 
        :: Vector Int -> Ref Int
        -> RateNat k  -> Series k Int
        -> Process

filterMax v r _ s1
 = let  s2      = R.map (\x -> x + 1) s1
        s3      = R.map (\x -> x > 0) s2
   in   R.mkSel1 s3 
         (\sel -> let s4   = R.pack sel s2
                  in  R.fill v s4
                   %  R.reduce r maxx 0 s4)

maxx :: Int -> Int -> Int
maxx x y
 = if x > y then x else y
{-# INLINE [0] maxx #-}

