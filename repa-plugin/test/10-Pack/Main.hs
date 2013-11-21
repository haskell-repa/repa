{-# LANGUAGE MagicHash #-}
module Main where
import Data.Array.Repa.Series           as R
import Data.Array.Repa.Series.Series    as S
import Data.Array.Repa.Series.Vector    as V
import Data.Array.Repa.Series.Ref       as Ref
import Data.Array.Repa.Series.Ref       (Ref)
import qualified Data.Vector.Primitive  as P
import GHC.Exts

---------------------------------------------------------------------
-- | Set the primitives used by the lowering transform.
repa_primitives :: R.Primitives
repa_primitives =  R.primitives


---------------------------------------------------------------------
main
 = do   -------------------------------
        putStrLn "even"
        v       <- V.fromPrimitive $ P.enumFromN (1 :: Int) 10
        v1      <- V.fromPrimitive $ P.enumFromN (1 :: Int) 10
        R.runProcess v (evens v1)               -- TODO: slice to final length
        print v1

        -------------------------------
        putStrLn "\nevenSum"
        v1      <- V.fromPrimitive $ P.enumFromN (1 :: Int) 10
        r0      <- Ref.new 0
        r1      <- Ref.new 0
        R.runProcess v (evenSum v1 r0 r1)
        print v1                                -- TODO: slice to final length
        Ref.read r0 >>= print
        Ref.read r1 >>= print           

        -------------------------------
        putStrLn "\nevenSum2"
        v1      <- V.fromPrimitive $ P.enumFromN (1 :: Int) 10
        r0      <- Ref.new 0
        r1      <- Ref.new 0
        R.runProcess v (evenSum2 v1 r0 r1)
        print v1                                -- TODO: slice to final length
        Ref.read r0 >>= print
        Ref.read r1 >>= print

        -------------------------------
        putStrLn "\nevenMax"
        v1      <- V.fromPrimitive $ P.enumFromN (1 :: Int) 10
        r0      <- Ref.new 0
        R.runProcess v (evenMax v1 r0)
        print v1                                -- TODO: slice to final length
        Ref.read r0 >>= print

        -------------------------------
        putStrLn "\npartial"
        v1      <- V.fromPrimitive $ P.enumFromN (1 :: Int) 10
        R.runProcess v (partial 5 v1)           -- TODO: slice to final length
        print v1

        -------------------------------
        putStrLn "\npartitions"
        v1      <- V.fromPrimitive $ P.enumFromN (1 :: Int) 10
        v2      <- V.fromPrimitive $ P.enumFromN (1 :: Int) 10
        R.runProcess v (partitions v1 v2)       -- TODO: slice to final length
        print v1
        print v2


-- | Return just the even values.
evens   :: Vector Int 
        -> RateNat k -> R.Series k Int 
        -> Process

evens v1 _ s1
 = R.mkSel1 
        (R.map (\x -> x `mod` 2 == 0) s1)
        (\sel -> R.fill v1 (R.pack sel s1))


-- | Return the positive values, 
--   along with the sum of all, and sum of just positive.
evenSum :: Vector Int -> Ref Int -> Ref Int
        -> RateNat k  -> R.Series k Int 
        -> Process

evenSum v1 r0 r1 _ s1
 = R.reduce r0 (+) 0 s1
 % let  flags    = R.map (\x -> x `mod` 2 == 0) s1
   in   R.mkSel1 flags
         (\sel -> let sEven     = R.pack sel s1
                  in  R.fill   v1 sEven
                   %  R.reduce r1 (+) 0 sEven)


-- | As above, but making the total syntactically inside
--   the selector context.
evenSum2 :: Vector Int -> Ref Int -> Ref Int
         -> RateNat k  -> R.Series k Int 
         -> Process

evenSum2 v1 r0 r1 _ s1 
 = R.mkSel1 (R.map (\x -> x `mod` 2 == 0) s1)
   (\sel -> let sEven  = R.pack sel s1
            in  R.fill v1 sEven
             %  R.reduce r0 (+) 0 s1 
             %  R.reduce r1 (+) 0 sEven) 


-- | Get the vector of positive values,
--   as well as the maximual element.
evenMax :: Vector Int -> Ref Int
        -> RateNat k  -> R.Series k Int
        -> Process

evenMax v1 r0 _ s1
 = R.mkSel1 (R.map (\x -> x `mod` 2 == 0) s1)
   (\sel -> let sEven   = R.pack sel s1
            in  R.fill v1 sEven
              % R.reduce r0 maxx 0 sEven)

maxx :: Int -> Int -> Int
maxx x y
 = if x > y then x else y
{-# INLINE [0] maxx #-}


-- | Get all the values more than the given limit,
--   tests out passing configuration values to fusable function.
partial :: Int -> Vector Int 
        -> RateNat k -> R.Series k Int 
        -> Process

partial limit v1 _ s1
 = R.mkSel1 (R.map (\x -> x > limit) s1)
   (\sel -> R.fill v1 (R.pack sel (R.map (* 2) s1)))


-- | Partition a vector into two parts
partitions 
        :: Vector Int -> Vector Int
        -> RateNat k  -> R.Series k Int
        -> Process

partitions v1 v2 _ s1
 = let s2       = R.map (* 2) s1
   in  R.mkSel1 (R.map (\x -> x >  10) s2) (\sel1 ->
       R.mkSel1 (R.map (\x -> x <= 10) s2) (\sel2
        -> R.fill v1 (R.pack sel1 s2)
        %  R.fill v2 (R.pack sel2 s2)))

