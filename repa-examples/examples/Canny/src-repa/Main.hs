{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards, 
             MagicHash, ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

-- | Canny edge detector.
--
--   NOTE: for best performance this needs to be compiled with the following GHC options:
--         -fllvm -optlo-O3 -Odph -fno-liberate-case
--         -funfolding-use-threshold100 -funfolding-keeness-factor100
--
import Data.List
import Data.Word
import Data.Int
import Control.Monad
import System.Environment
import Data.Array.Repa                          as R
import Data.Array.Repa.Repr.Unboxed             as U
import Data.Array.Repa.Repr.Cursored            as C
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Data.Array.Repa.Specialised.Dim2
import Data.Array.Repa.Algorithms.Pixel
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing
import Debug.Trace
import GHC.Exts
import qualified Data.Vector.Unboxed.Mutable    as VM
import qualified Data.Vector.Unboxed            as V
import qualified Prelude                        as P
import Prelude                                  hiding (compare)


type Image a    = Array U DIM2 a

-- Constants ------------------------------------------------------------------
orientUndef     = 0     :: Word8
orientPosDiag   = 64    :: Word8
orientVert      = 128   :: Word8
orientNegDiag   = 192   :: Word8
orientHoriz     = 255   :: Word8

data Edge       = None | Weak | Strong
edge None       = 0     :: Word8
edge Weak       = 128   :: Word8
edge Strong     = 255   :: Word8


-- Main routine ---------------------------------------------------------------
main 
 = do   args    <- getArgs
        case args of
         [fileIn, fileOut]              
           -> run 0 50 100 fileIn fileOut

         [loops, threshLow, threshHigh, fileIn, fileOut]
           -> run (read loops) (read threshLow) (read threshHigh) fileIn fileOut

         _ -> putStrLn 
           $ concat [ "repa-canny [<loops::Int> <threshLow::Int> <threshHigh::Int>]"
                    , " <fileIn.bmp> <fileOut.bmp>" ]


run loops threshLow threshHigh fileIn fileOut
 = do   arrInput <- liftM (either (error . show) id) 
                 $  readImageFromBMP fileIn

        (arrResult, tTotal)
         <- time $ process loops threshLow threshHigh arrInput

        when (loops >= 1)
         $ putStrLn $ "\nTOTAL\n"
        
        putStr $ prettyTime tTotal
        
        writeImageToBMP fileOut (U.zip3 arrResult arrResult arrResult)


process loops threshLow threshHigh arrInput
 = do   arrGrey         <- timeStage loops "toGreyScale"
                        $  toGreyScale    arrInput

        arrBluredX      <- timeStage loops "blurX"
                        $  blurSepX arrGrey

        arrBlured       <- timeStage loops "blurY"
                        $  blurSepY arrBluredX


        arrDX           <- timeStage loops "diffX"        
                        $  gradientX arrBlured

        arrDY           <- timeStage loops "diffY"
                        $  gradientY arrBlured
                
        arrMagOrient    <- timeStage loops "magOrient"   
                        $  gradientMagOrient threshLow arrDX arrDY

        arrSuppress     <- timeStage loops "suppress"     
                        $  suppress threshLow threshHigh arrMagOrient

        arrStrong       <- timeStage loops "select"
                        $  selectStrong arrSuppress   

        arrEdges        <- timeStage loops "wildfire"
                        $  wildfire arrSuppress arrStrong     

        return arrEdges


-- | Wrapper to time each stage of the algorithm.
timeStage
        :: (Shape sh, Unbox a)
        => Int
        -> String 
        -> IO (Array U sh a)
        -> IO (Array U sh a)

timeStage loops name fn
 = do   
        let burn !n
             = do !arr  <- fn
                  if n <= 1 then return arr
                            else burn (n - 1)

        traceEventIO $ "**** Stage " P.++ name P.++ " begin."
                
        (arrResult, t)
         <- time $ do  !arrResult' <- burn loops
                       return arrResult'

        traceEventIO $ "**** Stage " P.++ name P.++ " end."

        when (loops >= 1) 
         $ putStr       $  name P.++ "\n"
                        P.++ unlines [ "  " P.++ l | l <- lines $ prettyTime t ]

        return arrResult
{-# NOINLINE timeStage #-}


-------------------------------------------------------------------------------
-- | RGB to greyscale conversion.
toGreyScale :: Image (Word8, Word8, Word8) -> IO (Image Float)
toGreyScale arr
        = computeP
        $ R.map (* 255)
        $ R.map floatLuminanceOfRGB8 arr 
{-# NOINLINE toGreyScale #-}


-- | Separable Gaussian blur in the X direction.
blurSepX :: Image Float -> IO (Image Float)
blurSepX arr
        = computeP
        $ forStencil2  BoundClamp arr
          [stencil2|    1 4 6 4 1 |]    
{-# NOINLINE blurSepX #-}


-- | Separable Gaussian blur in the Y direction.
blurSepY :: Image Float -> IO (Image Float)
blurSepY arr
        = computeP
        $ R.smap (/ 256)
        $ forStencil2  BoundClamp arr
          [stencil2|    1
                        4
                        6
                        4
                        1 |]
{-# NOINLINE blurSepY #-}


-- | Compute gradient in the X direction.
gradientX :: Image Float -> IO (Image Float)
gradientX img
        = computeP
        $ forStencil2 BoundClamp img
          [stencil2|    -1  0  1
                        -2  0  2
                        -1  0  1 |]
{-# NOINLINE gradientX #-}


-- | Compute gradient in the Y direction.
gradientY :: Image Float -> IO (Image Float)
gradientY img
        = computeP
        $ forStencil2 BoundClamp img
          [stencil2|     1  2  1
                         0  0  0
                        -1 -2 -1 |] 
{-# NOINLINE gradientY #-}


-- | Classify the magnitude and orientation of the vector gradient.
gradientMagOrient 
        :: Float -> Image Float -> Image Float -> IO (Image (Float, Word8))

gradientMagOrient !threshLow dX dY
        = computeP
        $ R.zipWith magOrient dX dY

 where  magOrient :: Float -> Float -> (Float, Word8)
        magOrient !x !y
                = (magnitude x y, orientation x y)
        {-# INLINE magOrient #-}
        
        magnitude :: Float -> Float -> Float
        magnitude !x !y
                = sqrt (x * x + y * y)
        {-# INLINE magnitude #-}
        
        {-# INLINE orientation #-}
        orientation :: Float -> Float -> Word8
        orientation !x !y

         -- Don't bother computing orientation if vector is below threshold.
         | x >= negate threshLow, x < threshLow
         , y >= negate threshLow, y < threshLow
         = orientUndef 

         | otherwise
         = let  -- Determine the angle of the vector and rotate it around a bit
                -- to make the segments easier to classify.
                !d      = atan2 y x 
                !dRot   = (d - (pi/8)) * (4/pi)
        
                -- Normalise angle to beween 0..8
                !dNorm  = if dRot < 0 then dRot + 8 else dRot

                -- Doing explicit tests seems to be faster than using the FP floor function.
           in fromIntegral 
               $ I# (if dNorm >= 4
                     then if dNorm >= 6
                          then if dNorm >= 7
                                then 255#               -- 7
                                else 192#               -- 6

                          else if dNorm >= 5
                                then 128#               -- 5
                                else 64#                -- 4

                     else if dNorm >= 2
                        then if dNorm >= 3
                                then 255#               -- 3
                                else 192#               -- 2

                        else if dNorm >= 1
                                then 128#               -- 1
                                else 64#)               -- 0
{-# NOINLINE gradientMagOrient #-}


-- | Suppress pixels that are not local maxima, and use the magnitude to classify maxima
--   into strong and weak (potential) edges.
suppress :: Float -> Float -> Image (Float, Word8) -> IO (Image Word8)
suppress !threshLow !threshHigh !dMagOrient
 = computeP
 $ makeBordered2 
        (extent dMagOrient) 1 
        (makeCursored (extent dMagOrient) id addDim comparePts)
        (fromFunction (extent dMagOrient) (const 0))

 where  {-# INLINE comparePts #-}
        comparePts d@(sh :. i :. j)
         | o == orientUndef     = edge None
         | o == orientHoriz     = isMax (getMag (sh :. i   :. j-1)) (getMag (sh :. i   :. j+1)) 
         | o == orientVert      = isMax (getMag (sh :. i-1 :. j))   (getMag (sh :. i+1 :. j)) 
         | o == orientNegDiag   = isMax (getMag (sh :. i-1 :. j-1)) (getMag (sh :. i+1 :. j+1)) 
         | o == orientPosDiag   = isMax (getMag (sh :. i-1 :. j+1)) (getMag (sh :. i+1 :. j-1)) 
         | otherwise            = edge None
      
         where
          !o            = getOrient d  
          !m            = getMag    (Z :. i :. j)

          getMag        = fst . (R.unsafeIndex dMagOrient)
          getOrient     = snd . (R.unsafeIndex dMagOrient)

          {-# INLINE isMax #-}
          isMax !intensity1 !intensity2
            | m < threshLow     = edge None
            | m < intensity1    = edge None
            | m < intensity2    = edge None
            | m < threshHigh    = edge Weak
            | otherwise         = edge Strong
{-# NOINLINE suppress #-}


-- | Select indices of strong edges.
--   TODO: If would better if we could medge this into the above stage, and
--         record the strong edge during non-maximum suppression, but Repa
--         doesn't provide a fused mapFilter primitive yet.
selectStrong :: Image Word8 -> IO (Array U DIM1 Int)
selectStrong img
 = let  vec             = toUnboxed img

        match ix        = vec `V.unsafeIndex` ix == edge Strong
        {-# INLINE match #-}
        
        process' ix     = ix
        {-# INLINE process' #-}
        
   in   selectP match process' (size $ extent img)
{-# NOINLINE selectStrong #-}


-- | Trace out strong edges in the final image. 
--   Also trace out weak edges that are connected to strong edges.
wildfire 
        :: Image Word8       -- ^ Image with strong and weak edges set.
        -> Array U DIM1 Int  -- ^ Array containing flat indices of strong edges.
        -> IO (Image Word8)

wildfire img arrStrong
 = do   (sh, vec)       <- wildfireIO 
        return  $ sh `seq` vec `seq` fromUnboxed sh vec

 where  lenImg          = R.size $ R.extent img
        lenStrong       = R.size $ R.extent arrStrong
        vStrong         = toUnboxed arrStrong
        
        wildfireIO
         = do   -- Stack of image indices we still need to consider.
                vStrong' <- V.thaw vStrong
                vStack   <- VM.grow vStrong' (lenImg - lenStrong)
        
                -- Burn in new edges.
                vImg    <- VM.unsafeNew lenImg
                VM.set vImg 0
                burn vImg vStack lenStrong
                vImg'   <- V.unsafeFreeze vImg
                return  (extent img, vImg')

        
        burn :: VM.IOVector Word8 -> VM.IOVector Int -> Int -> IO ()
        burn !vImg !vStack !top
         | top == 0
         = return ()
        
         | otherwise
         = do   let !top'               =  top - 1
                n                       <- VM.unsafeRead vStack top'
                let (Z :. y :. x)       = fromIndex (R.extent img) n

                let {-# INLINE push #-}
                    push t              = pushWeak vImg vStack t
                                
                VM.write vImg n (edge Strong)
                 >>  push (Z :. y - 1 :. x - 1) top'
                 >>= push (Z :. y - 1 :. x    )
                 >>= push (Z :. y - 1 :. x + 1)

                 >>= push (Z :. y     :. x - 1)
                 >>= push (Z :. y     :. x + 1)

                 >>= push (Z :. y + 1 :. x - 1)
                 >>= push (Z :. y + 1 :. x    )
                 >>= push (Z :. y + 1 :. x + 1)

                 >>= burn vImg vStack

        -- If this ix is weak in the source then set it to strong in the
        -- result and push the ix onto the stack.
        {-# INLINE pushWeak #-}
        pushWeak vImg vStack ix top
         = do   let n           = toIndex (extent img) ix
                xDst            <- VM.unsafeRead vImg n
                let xSrc        = img `R.unsafeIndex` ix

                if   xDst == edge None 
                  && xSrc == edge Weak
                 then do
                        VM.unsafeWrite vStack top (toIndex (extent img) ix)
                        return (top + 1)
                        
                 else   return top
{-# NOINLINE wildfire #-}


