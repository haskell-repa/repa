{-# LANGUAGE MagicHash, RankNTypes, ExistentialQuantification #-}
module Main where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import qualified Data.Vector.Unboxed    as U

import GHC.Exts

import System.IO.Unsafe
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
                   _       -> error "Usage: quickselect <size>"
        v1 <- V.fromUnboxed $ gen 23489 sz
        print $ R.runSeries v1 quickselect -- quickselect v1


-- incredibly dodgy number generator
gen :: Int -> Int -> U.Vector Int
gen seed size
 = U.generate size r
 where
  r i = i * (seed*5319) `mod` (seed * 978) `mod` 500

{-
-- Doesn't work: loop comes before "let p = S.index ..."
quickselect :: R.Vector Int -> Int
quickselect xs
 = go xs (I# (V.length xs `quotInt#` 2#))
 where
  go xs i =
   let (xs',i') = R.runSeries xs (flip lower_go i)
   in  if   V.length xs' ==# 1#
       then ix xs' 0
       else go xs' i'
  ix v i = unsafePerformIO (do
                v' <- V.toUnboxed v
                U.indexM v' i
            )

lower_go :: R.Series k Int -> Int -> (R.Vector Int, Int)
lower_go xs i
 = let p  = S.index xs 0#
       lt = R.map (< p) xs
       gt = R.map (> p) xs
       count_true x = case x of
                        True  -> I# 1#
                        False -> I# 0#
       ltL= R.fold (+) 0 (R.map count_true lt)
       len= I# (S.length xs)
       gtL= R.fold (+) 0 (R.map count_true gt) - len
   in  if      i < ltL
       then (R.mkSel1 lt (\ltS -> S.toVector (R.pack ltS xs)), i)
       else if i >= gtL
       then (R.mkSel1 gt (\gtS -> S.toVector (R.pack gtS xs)), i - gtL)
       else (mkVec p, 0)

mkVec p = unsafePerformIO (V.fromUnboxed (U.fromList [p]))
-}


quickselect :: R.Series k Int -> Int
quickselect xs
 = go (S.length xs `quotInt#` 2#) xs
 where
  go :: Int# -> R.Series k' Int -> Int
  go i xs
   = let p       = S.index xs 0#
         (lt,gt) = lower_filters xs p
         ltL     = V.length lt
         gtL     = S.length xs -# V.length gt
     in  if      i <#  ltL
         then R.runSeries lt (go i)
         else if i >=# gtL
         then R.runSeries gt (go (i -# gtL))
         else p

lower_filters :: R.Series k Int -> Int -> (V.Vector Int, V.Vector Int)
lower_filters xs p
 = let lt = R.map (< p) xs
       gt = R.map (> p) xs
   in  R.mkSel1 lt (\ltS ->
       R.mkSel1 gt (\gtS ->
         (S.toVector (R.pack ltS xs), S.toVector (R.pack gtS xs))))

{-
-- Doesn't work: no conversion for cases
quickselect :: R.Series k Int -> Int
quickselect xs
 = lower_go xs (S.length xs `quotInt#` 2#)

lower_go :: R.Series k' Int -> Int# -> Int
lower_go xs i
 = let p  = S.index xs 0#
       lt = R.map (< p) xs
       gt = R.map (> p) xs
   in  R.mkSel1 lt (\ltS ->
       R.mkSel1 gt (\gtS ->
         let lt' = R.pack ltS xs
             ltL = S.length lt'
             gt' = R.pack gtS xs
             gtL = S.length xs -# S.length gt'
         in  if      i <#  ltL
             then lower_go lt' i
             else if i >=# gtL
             then lower_go gt' (i -# gtL)
             else p))
-}
{-
 - -- Doesn't work: no append yet
quicksort :: R.Series k Int -> Vector Int
quicksort xs
 = S.toVector (go xs)
 where
  go xs
   | S.length xs < 1
   = xs
   | otherwise
   = let p  = S.index xs 0
         lt = S.map (< p) xs
         eq = S.map (==p) xs
         gt = S.map (> p) xs
     in  S.mkSel1 lt $ \lt' ->
         S.mkSel1 eq $ \eq' ->
         S.mkSel1 gt $ \gt' ->
           go (pack lt' xs) `app` pack eq xs `app` go (pack gt' xs)
-} 
