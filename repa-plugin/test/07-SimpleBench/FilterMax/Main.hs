
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
 = do   args    <- getArgs
        let sz  = case args of
                   [szStr] -> (Prelude.read szStr :: Int)
                   _       -> error "Usage: simplebench <size>"

        vec             <- V.fromUnboxed $ U.enumFromN (0 :: Int) sz
        let (vec', mx)  = R.runSeries vec lower_maxx
        print (vec', mx)


-- | Get the vector of positive values,
--   as well as the maximual element.
lower_maxx :: Series k Int -> (Vector Int, Int)
lower_maxx s1
 = R.mkSel1 (R.map (\x -> x `mod` 2 == 0) s1)
   (\sel -> let sEven   = R.pack sel s1
            in  ( S.toVector sEven
                , R.fold maxx 0 sEven))

maxx :: Int -> Int -> Int
maxx x y
 = if x > y then x else y
{-# INLINE [0] maxx #-}
