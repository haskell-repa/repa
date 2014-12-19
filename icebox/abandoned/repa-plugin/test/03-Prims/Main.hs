
module Main where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Ref       as Ref
import Data.Array.Repa.Series.Ref       (Ref)
import qualified Data.Vector.Primitive  as P

---------------------------------------------------------------------
-- | Set the primitives used by the lowering transform.
repa_primitives :: R.Primitives
repa_primitives =  R.primitives


---------------------------------------------------------------------
main
 = do   -------------------------------
        putStrLn "Int"
        v1_Int    <- V.fromPrimitive $ P.enumFromN (1 :: Int) 10
        r0_Int    <- Ref.new 0
        r1_Int    <- Ref.new 1
        R.runProcess v1_Int   (ffold_Int r0_Int r1_Int)
        Ref.read r0_Int >>= print
        Ref.read r1_Int >>= print

        -------------------------------
        putStrLn "\nFloat"
        v1_Float  <- V.fromPrimitive $ P.enumFromN (1 :: Float) 10
        r0_Float  <- Ref.new 0
        r1_Float  <- Ref.new 1
        R.runProcess v1_Float  (ffold_Float r0_Float r1_Float)
        Ref.read r0_Float >>= print
        Ref.read r1_Float >>= print

        -------------------------------
        putStrLn "\nDouble"
        v1_Double <- V.fromPrimitive $ P.enumFromN (1 :: Double) 10
        r0_Double <- Ref.new 0
        r1_Double <- Ref.new 1
        R.runProcess v1_Double (ffold_Double r0_Double r1_Double)
        Ref.read r0_Double >>= print
        Ref.read r1_Double >>= print


-- Int
ffold_Int    
        :: Ref Int -> Ref Int
        -> RateNat k -> Series k Int -> Process
ffold_Int    r0 r1 _ s
 = R.reduce r0 (+) 0 s 
 % R.reduce r1 (*) 1 s


-- Float
ffold_Float  
        :: Ref Float -> Ref Float
        -> RateNat k -> Series k Float -> Process
ffold_Float  r0 r1 _ s
 = R.reduce r0 (+) 0 s
 % R.reduce r1 (*) 1 s


-- Double
ffold_Double 
        :: Ref Double -> Ref Double
        -> RateNat k -> Series k Double -> Process
ffold_Double  r0 r1 _ s
 = R.reduce r0 (+) 0 s
 % R.reduce r1 (*) 1 s


