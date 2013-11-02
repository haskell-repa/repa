{-# LANGUAGE MagicHash, RankNTypes #-}
module Main (main, repa_primitives) where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import qualified Data.Vector.Primitive  as P
import Data.Array.Repa.IO.Timing

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
                   _       -> error "Usage: mapmap <size>"

        i1      <- V.fromPrimitive $ P.enumFromN 0 sz
        o1      <- (V.new sz :: IO (V.Vector Float))
        o2      <- (V.new sz :: IO (V.Vector Float))
        i1 `seq` o1 `seq` o2 `seq` return ()

        ((), t) <- time $ runProcess i1 (lower_mapmap o1 o2)
        o1'     <- V.toPrimitive o1
        o2'     <- V.toPrimitive o2

        putStr (prettyTime t)
        print (P.head o1', P.length o1', P.head o2', P.length o2')


lower_mapmap  :: forall k
        .  R.Vector Float -> R.Vector Float
        -> RateNat k
        -> R.Series k Float 
        -> Process

lower_mapmap o1 o2 _ xs
 = let xs' = R.map (\x -> x * 2)  xs
       ys  = R.map (\x -> x + 50) xs'
       zs  = R.map (\x -> x - 50) xs'
   in  
        R.fill o1 ys
     %  R.fill o2 zs
{-# NOINLINE lower_mapmap #-}
