module Main where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import qualified Data.Vector.Primitive  as P

---------------------------------------------------------------------
-- | Set the primitives used by the lowering transform.
repa_primitives :: R.Primitives
repa_primitives =  R.primitives


---------------------------------------------------------------------
-- | The opaque worker function for map
-- Noinline, because we want to make sure lower can handle arbitrary functions
{-# NOINLINE f_opaque #-}
f_opaque :: Int -> Int
f_opaque x = x + 1


---------------------------------------------------------------------
main
 = do   v       <- V.fromPrimitive $ P.enumFromN (1 :: Int) 10

        -------------------------------
        putStrLn "singleMap"
        v1      <- V.fromPrimitive $ P.replicate 10 0
        R.runProcess v (singleMap v1)
        print v1

        -------------------------------
        putStrLn "\nexternMap"
        v1      <- V.fromPrimitive $ P.replicate 10 0
        R.runProcess v (externMap v1)
        print v1



-- Map with the opaque function defined in this module
singleMap :: Vector Int 
          -> RateNat k -> R.Series k Int -> Process
singleMap v _ s
 = R.fill v (R.map f_opaque s)


-- Map with an opaque function defined in another module
externMap :: Vector Int
          -> RateNat k -> R.Series k Int -> Process
externMap v _ s
 = R.fill v (R.map ([20..] !!) s)
