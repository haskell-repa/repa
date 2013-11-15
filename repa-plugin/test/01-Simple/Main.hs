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
 = do   v       <- V.fromPrimitive $ P.enumFromN (1 :: Int) 10

        -------------------------------
        putStrLn "single"
        r0      <- Ref.new 0
        R.runProcess v (single r0)
        Ref.read r0 >>= print

        -------------------------------
        putStrLn "\nffold"
        r0      <- Ref.new 0
        r1      <- Ref.new 1
        R.runProcess v (ffold r0 r1)
        Ref.read r0 >>= print
        Ref.read r1 >>= print

        -------------------------------
        putStrLn "\nfffold"
        r0      <- Ref.new 0
        r1      <- Ref.new 1
        r2      <- Ref.new 1
        R.runProcess v (fffold r0 r1 r2)
        Ref.read r0 >>= print
        Ref.read r1 >>= print
        Ref.read r2 >>= print

        -------------------------------
        putStrLn "\nfoldMap"
        r0      <- Ref.new 0
        R.runProcess v (foldMap r0)
        Ref.read r0 >>= print

        -------------------------------
        putStrLn "\nsingleMap"
        v1      <- V.fromPrimitive $ P.replicate 10 0
        R.runProcess v (singleMap v1)
        print v1

        -------------------------------
        putStrLn "\ndoubleMap"
        v1      <- V.fromPrimitive $ P.replicate 10 0
        R.runProcess v (doubleMap v1)
        print v1


-- Single fold.
single  :: Ref Int 
        -> RateNat k -> Series k Int -> Process
single r _ s
        = R.reduce r (+) 0 s


-- Double fold fusion.
--  Computation of both reductions is interleaved.
ffold   :: Ref Int -> Ref Int 
        -> RateNat k -> Series k Int -> Process
ffold r0 r1 _ s
        = R.reduce r0 (+) 0 s
        % R.reduce r1 (*) 1 s


-- Triple fold fusion.
--  We end up with an extra let-binding for the second baseband 
--  addition that needs to be handled properly.
fffold  :: Ref Int -> Ref Int -> Ref Int
        -> RateNat k -> Series k Int -> Process
fffold r0 r1 r2 _ s
        = R.reduce r0 (+) 0 s 
        % R.reduce r1 (*) 1 s
        % R.reduce r2 (*) 2 s


-- Fold/map fusion.
foldMap :: Ref Int
        -> RateNat k -> Series k Int -> Process
foldMap ref _ s
        = R.reduce ref (+) 0 (R.map (\x -> x * 2) s)


-- Single maps
--  The resulting code produces a vector rather than a plain Int.
singleMap 
        :: Vector Int
        -> RateNat k -> Series k Int -> Process
singleMap v1 _ s
        = R.fill v1 (R.map (\x -> x * 2 + 1) s)


-- Map/map fusion,
--  Producing a vector.
doubleMap  :: Vector Int
        -> RateNat k -> Series k Int -> Process
doubleMap v1 _ s
        = R.fill v1 (R.map (\x -> x * 2) (R.map (\x -> x + 1) s))


