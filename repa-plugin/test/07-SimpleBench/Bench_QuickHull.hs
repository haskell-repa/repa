{-# LANGUAGE MagicHash, RankNTypes #-}
module Main where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import qualified Data.Vector.Unboxed    as U

import GHC.Exts
import System.Environment

---------------------------------------------------------------------
-- | Set the primitives used by the lowering transform.
repa_primitives :: R.Primitives
repa_primitives =  R.primitives

---------------------------------------------------------------------
main
 = do   args <- getArgs
        let sz = case args of
                   [szStr] -> (Prelude.read szStr :: Int)
                   _       -> error "Usage: simplebench <size>"
        let pts = U.enumFromN (0 :: Int) sz `U.zip` U.enumFromN (0 :: Int) sz
        pts' <- quickhull pts
        print pts'


quickhull :: U.Vector (Int,Int)
          -> IO (U.Vector (Int,Int))
quickhull ps
 | U.length ps == 0
 = return U.empty
 | otherwise
 = do   v <- V.fromUnboxed ps
        xs <- V.fromUnboxed (U.map fst ps)
        ys <- V.fromUnboxed (U.map snd ps)
        let (ix,iy)     = ps U.! 0
            Just (imin,imax) = R.runSeries2 xs ys (lower_minmax ix iy)
            pmin        = ps U.! imin
            pmax        = ps U.! imax

        u1 <- hsplit v pmin pmax
        u2 <- hsplit v pmax pmin
        return (u1 U.++ u2)
 where
  hsplit v p1@(x1,y1) p2@(x2,y2)
   = do  let (packed,pm) = R.runSeries v (zlower_filtermax x1 y1 x2 y2)
         case V.length packed <# 2# of
          True
           -> do upacked <- V.toUnboxed packed
                 return ((x1,x2) `U.cons` upacked)
          False
           -> do u1 <- hsplit packed p1 pm
                 u2 <- hsplit packed pm p2
                 return (u1 U.++ u2)


lower_minmax :: Int -> Int
             -> R.Series k Int
             -> R.Series k Int
             -> (Int, Int)
lower_minmax ix iy xs ys
 = let (_,imin) = R.foldIndex (\i (x',i') x -> if x < x' then (x, I# i) else (x',i')) (ix,0) xs
       (_,imax) = R.foldIndex (\i (x',i') x -> if x > x' then (x, I# i) else (x',i')) (ix,0) xs
   in  (imin,imax)


zlower_filtermax
             :: Int -> Int
             -> Int -> Int
             -> R.Series k (Int, Int)
             -> (R.Vector (Int,Int), (Int,Int))
zlower_filtermax x1 y1 x2 y2 vs
 = let cs    = R.map    (\(x,y) -> ((x,y), (x1-x)*(y2-y) - (y1-y)*(x2-x))) vs
       pmax  = R.fold   (\((x,y),d) ((x',y'),d') -> if d > d' then ((x,y),d) else ((x',y'),d')) ((0,0),0) cs
       pack  = R.map    (\((x,y),d) -> d > 0) cs
   in R.mkSel1 pack (\sel ->
       let ps'   = R.pack sel vs
           (pm,_)= pmax
       in  (S.toVector ps', pm))


