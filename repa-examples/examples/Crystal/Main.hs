
-- Quasicrystals demo. 
--  
-- Based on code from:
--   http://mainisusuallyafunction.blogspot.com/2011/10/quasicrystals-as-sums-of-waves-in-plane.html

{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
import Data.Array.Repa ( Array, DIM2, Z(..), (:.)(..), U)
import qualified Data.Array.Repa          as R
import qualified Data.Array.Repa.IO.BMP   as R

import Data.Word  ( Word8   )

type R     = Float
data R2    = R2 !R !R
type Angle = R

pixels :: Int
pixels = 800

scale :: R
scale = 128


{-# INLINE point #-}
point :: DIM2 -> R2
point = \(Z :. x :. y) -> R2 (adj x) (adj y) where
    !denom       = fromIntegral pixels - 1

    {-# INLINE adj #-}
    adj n       = scale * ((2 * fromIntegral n / denom) - 1)


{-# INLINE waves #-}
waves :: R2 -> R
waves x = wrap $ waver 0 7
 where
    {-# INLINE waver #-}
    waver :: Float -> Int -> Float
    waver !acc !n
     | n == 0    = acc
     | otherwise = waver (acc + wave (fromIntegral n * th) x) (n - 1)
     
    !th = pi / fromIntegral (7 :: Int)
    
    {-# INLINE wrap #-}
    wrap n 
     = let !n_  = truncate n :: Int
           !n'  = n - fromIntegral n_
       in  if odd n_ then 1 - n'
                     else n'


{-# INLINE wave #-}
wave :: Angle -> R2 -> R
wave th = f where
    !cth  = cos th
    !sth  = sin th

    {-# INLINE f #-}
    f (R2 x y)  = (cos (cth*x + sth*y) + 1) / 2


{-# INLINE quasicrystal #-}
quasicrystal :: DIM2 -> R
quasicrystal p
        = waves $ point p 


{-# INLINE word8OfFloat #-}
word8OfFloat :: Float -> Word8
word8OfFloat f
 = fromIntegral (truncate f :: Int) 


{-# NOINLINE makeImage #-}
makeImage :: Array U DIM2 (Word8, Word8, Word8)
makeImage         
        = R.compute
        $ R.map (\x -> (x, x, x)) 
        $ R.map (word8OfFloat . (*255) . min 1 . max 0) 
        $ R.fromFunction (Z :. pixels :. pixels) quasicrystal


main :: IO ()
main = do
    R.writeImageToBMP "out.bmp" makeImage

