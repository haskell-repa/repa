{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

-- | Apply Sobel operators to an image.
import System.Environment
import Data.Array.Repa                          as R
import Data.Array.Repa.Algorithms.Pixel         as R
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing
import Prelude                          hiding (compare)
import Control.Monad
import Solver
import Debug.Trace

main 
 = do   args    <- getArgs
        case args of
         [iterations, fileIn, fileOut]  
                -> run (read iterations) fileIn fileOut
         _      -> putStrLn "Usage: sobel <iterations::Int> <fileIn.bmp> <fileOut.bmp>"



run iterations fileIn fileOut
 = do   -- Load the source image and convert it to greyscale
        traceEventIO "******** Sobel Read Image"
        inputImage      <- liftM (either (error . show) id) 
                        $ readImageFromBMP fileIn

        traceEventIO "******** Sobel Luminance"
        (greyImage :: Array U DIM2 Float)
                        <- computeP
                        $  R.map floatLuminanceOfRGB8 inputImage
                
        -- Run the filter.
        traceEventIO "******** Sobel Loop Start"
        ((gX, gY), tElapsed)
                       <- time $ loop iterations greyImage

        traceEventIO "******** Sobel Loop End"
        putStr $ prettyTime tElapsed
        
        -- Write out the magnitute of the vector gradient as the result image.
        traceEventIO "******** Sobel Magnitude"
        outImage       <- computeUnboxedP
                       $  R.map rgb8OfGreyFloat  
                       $  R.map (/ 3)
                       $  R.zipWith magnitude gX gY     

        traceEventIO "******** Sobel Write Image"
        writeImageToBMP fileOut outImage


loop :: Int -> Image -> IO (Image, Image)
loop n img
 = img `deepSeqArray`
   if n == 0
    then return (img, img)
    else do 
        traceEventIO $ "******** Sobel Loop " Prelude.++ show n
        gX      <- gradientX img
        gY      <- gradientY img        
        if (n == 1) 
                then return (gX, gY)
                else loop (n - 1) img


-- | Determine the squared magnitude of a vector.
magnitude :: Float -> Float -> Float
{-# INLINE magnitude #-}
magnitude x y
        = sqrt (x * x + y * y)

