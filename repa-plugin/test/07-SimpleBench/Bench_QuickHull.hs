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
 = do   let (uxs, uys) = U.unzip ps
        xs <- V.fromUnboxed uxs
        ys <- V.fromUnboxed uys
        let (ix,iy)     = ps U.! 0
            Just ((_,imin),(_,imax)) = R.runSeries2 xs ys (lower_minmax ix iy)
            pmin        = ps U.! imin
            pmax        = ps U.! imax

        u1 <- hsplit xs ys pmin pmax
        u2 <- hsplit xs ys pmax pmin
        return (uncurry U.zip u1 U.++ uncurry U.zip u2)
 where
  hsplit xs ys p1@(x1,y1) p2@(x2,y2)
   = do  let Just (pxs,pys,(_,im)) = R.runSeries2 xs ys (lower_filtermax x1 y1 x2 y2)
         upxs <- V.toUnboxed pxs
         upys <- V.toUnboxed pys
         case V.length pxs <# 2# of
          True
           ->    return (x1 `U.cons` upxs, y1 `U.cons` upys)
          False
           -> do let pm = (upxs U.! im, upys U.! im)
                 (ux1,uy1) <- hsplit pxs pys p1 pm
                 (ux2,uy2) <- hsplit pxs pys pm p2
                 return (ux1 U.++ ux2, uy1 U.++ uy2)


lower_minmax :: Int -> Int
             -> R.Series k Int
             -> R.Series k Int
             -> ((Int,Int), (Int,Int))
lower_minmax ix iy xs ys
 = let imin = R.foldIndex (\i (x',i') x -> if x < x' then (x, I# i) else (x',i')) (ix,0) xs
       imax = R.foldIndex (\i (x',i') x -> if x > x' then (x, I# i) else (x',i')) (ix,0) xs
   in  (imin,imax)


lower_filtermax
             :: Int -> Int
             -> Int -> Int
             -> R.Series k Int
             -> R.Series k Int
             -> (R.Vector Int, R.Vector Int, (Int,Int))
lower_filtermax x1 y1 x2 y2 xs ys
 = let cs    = R.map2   (\x y -> (x1-x)*(y2-y) - (y1-y)*(x2-x)) xs ys
--       pmax  = R.fold   (\((x,y),d) ((x',y'),d') -> if d > d' then ((x,y),d) else ((x',y'),d')) ((0,0),0) cs
       pack  = R.map    (\d -> d > 0) cs
   in R.mkSel1 pack (\sel ->
       let xs'   = R.pack sel xs
           ys'   = R.pack sel ys
           cs'   = R.pack sel cs
           pmax  = R.foldIndex (\i (c',i') c -> if c > c' then (c, I# i) else (c', i')) (0,0) cs'
           -- (pm,_)= pmax
       in  (S.toVector xs', S.toVector ys', pmax))


