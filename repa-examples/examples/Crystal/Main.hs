
-- Quasicrystals demo. 
--  
-- Based on code from:
--   http://mainisusuallyafunction.blogspot.com/2011/10/quasicrystals-as-sums-of-waves-in-plane.html
--
{-# LANGUAGE BangPatterns #-}
import Data.Array.Repa ( Array, DIM2, Z(..), (:.)(..), D)
import Data.Array.Repa.Repr.ForeignPtr                  (F)
import qualified Data.Array.Repa.Repr.ForeignPtr        as R
import qualified Data.Array.Repa                        as R
import qualified Graphics.Gloss                         as G
import Data.Word 
import System.Environment

-- Types ----------------------------------------------------------------------
-- | Real value
type R      = Float

-- | Point on the 2D plane.
data R2     = R2 !R !R

-- | Angle in radians.
type Angle  = R

-- | Angle offset used for animation.
type Phi    = Float

-- | Number of waves to sum for each pixel.
type Degree = Int

-- | Feature size of visualisation.
type Scale  = Float

-- | Size of image to render.
type Size   = Int

-- | How many times to duplicate each pixel / image zoom.
type Zoom   = Int


-- Point ----------------------------------------------------------------------
-- | Compute a single point of the visualisation.
quasicrystal :: Size -> Scale -> Degree -> Phi -> DIM2 -> R
quasicrystal !size !scale !degree !phi !p
        = waves degree phi $ point size scale p 


-- | Sum up all the waves at a particular point.
waves :: Degree -> Phi -> R2 -> R
waves !degree !phi !x = wrap $ waver 0 degree
 where
    waver :: Float -> Int -> Float
    waver !acc !n
     | n == 0    = acc
     | otherwise = waver (acc + wave (fromIntegral n * th) x) (n - 1)
     
    !th = pi / phi
    
    wrap n 
     = let !n_  = truncate n :: Int
           !n'  = n - fromIntegral n_
       in  if odd n_ then 1 - n'
                     else n'


-- | Generate the value for a single wave.
wave :: Angle -> R2 -> R
wave !th = f where
    !cth  = cos th
    !sth  = sin th

    {-# INLINE f #-}
    f (R2 x y)  = (cos (cth*x + sth*y) + 1) / 2


-- | Convert an image point to a point on our wave plane.
point :: Size -> Scale -> DIM2 -> R2
point !size !scale (Z :. x :. y) 
 = R2 (adj x) (adj y)
 where
    !denom       = fromIntegral size - 1

    {-# INLINE adj #-}
    adj n       = scale * ((2 * fromIntegral n / denom) - 1)


-- Computation ----------------------------------------------------------------
-- | Compute a single frame as a wrapped ForeignPtr.
makeImage :: Size -> Scale -> Degree -> Phi -> Array F DIM2 Word8
makeImage !size !scale !degree !phi
 = let  
        -- Compute [0..1] values for the wave density at each point.
        arrVals   :: Array D DIM2 Float
        arrVals   = R.map (min 1 . max 0) 
                        $ R.fromFunction
                                (Z :. size :. size)
                                (quasicrystal size scale degree phi)

        -- Convert the [0..1] values to RGB Float colors.
        arrColors :: Array D DIM2 (Float, Float, Float)
        arrColors = R.map rampColor arrVals

        -- Convert the RGB Float colors to a flat image.
        arrPixels :: Array D DIM2 Word8
        arrPixels       
         = R.traverse   
                arrColors
                (\(Z :. height :. width) -> Z :. height :. width * 4)
                (\get (Z :. y :. x) 
                   -> let (r, g, b)     = get (Z :. y :. x `div` 4)
                      in  case x `mod` 4 of
                              0 -> 255
                              1 -> word8OfFloat (r * 255)
                              2 -> word8OfFloat (g * 255)
                              3 -> word8OfFloat (b * 255)
                              _ -> 0)
   in   R.compute arrPixels


-- | Color ramp from blue to white.
rampColor :: Float -> (Float, Float, Float)
rampColor v
 = (1, 0.4 + (v * 0.6), v)


-- | Float to Word8 conversion because the one in the GHC libraries
--   doesn't have enout specialisations and goes via Integer.
{-# INLINE word8OfFloat #-}
word8OfFloat :: Float -> Word8
word8OfFloat f
        = fromIntegral (truncate f :: Int) 


-- Rendering ------------------------------------------------------------------
-- | Compute a single frame of the animation as a Gloss picture.
{-# NOINLINE frame #-}
frame :: Size -> Scale -> Zoom -> Degree -> Float -> G.Picture
frame !size !scale !zoom !degree !time
 = let  
        -- Scale the time to be the phi value of the animation.
        -- The action seems to slow down at increasing phi values, 
        -- so we increase phi faster as time moves on.
        x       = 1 + (time ** 1.5) * 0.005

        -- 
        arr8 :: Array F DIM2 Word8
        arr8    = makeImage size scale degree x

        -- Wrap the ForeignPtr from the Array as a gloss picture.
        pic     = G.bitmapOfForeignPtr
                        size size               -- raw image size
                        (R.toForeignPtr arr8)   -- the image data.
                        False                   -- don't cache this in texture memory.

        zoom'  = fromIntegral zoom
        
        -- Zoom the image so we get a bigger window.
   in   G.Scale zoom' zoom' pic
   
   
-- Main -----------------------------------------------------------------------
main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         []     -> run 200 3 30 5

         [size, zoom, scale, degree]
                -> run (read size) (read zoom) (read scale) (read degree)

         _ -> putStr $ unlines
           [ "quazicrystal <size::Int> <zoom::Int> <scale::Float> <degree::Int>"
           , "    size    - visualisation size                  (default 200)"
           , "    zoom    - pixel replication factor            (default 3)"
           , "    scale   - feature size of visualisation       (default 30)"
           , "    degree  - number waves to sum for each point  (default 5)" 
           , ""
           , " You'll want to run this with +RTS -N to enable threads" ]
   

run :: Size -> Int -> Scale -> Degree -> IO ()                     
run size zoom scale degree
 = G.animateInWindow
                "Quasicrystals"
                (size  * zoom, size * zoom)
                (10, 10)
                G.black
                (frame size scale zoom degree)

