{-# LANGUAGE ScopedTypeVariables #-}

-- | Perform the 2D FFT on a BMP image.
import Data.Array.Repa.Algorithms.FFT
import Data.Array.Repa.Algorithms.DFT.Center
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa.Algorithms.Pixel
import Data.Array.Repa.IO.BMP
import System.Environment
import Control.Monad
import Data.Array.Repa                          as R

import Data.Word

main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         [fileIn, clipMag, fileMag, filePhase]
           -> mainWithArgs fileIn (read clipMag) fileMag filePhase

         _ -> putStr $ unlines
                [ "Usage: repa-fft2d <fileIn.bmp> <clip mag :: Int> <fileOutMag.bmp> <fileOutPhase.bmp>"
                , " "
                , "    Image dimensions must be powers of two, eg 128x512 or 64x256"
                , ""
                , "    The output magnitude has a high dynamic range. We need to clip it otherwise"
                , "    most of the pixels in the output BMP will be black. Start with a value equal"
                , "    to about the width of the image (eg 512)"
                , "" ]

        
mainWithArgs fileIn (clipMag :: Double) fileMag filePhase
 = do   
        -- Load in the matrix.
        arrRGB          <- liftM (either (\e -> error $ show e) id)
                        $  readImageFromBMP fileIn
        
        arrComplex      <- computeUnboxedP
                        $  R.map (\r -> (r, 0 :: Double)) 
                        $  R.map doubleLuminanceOfRGB8 arrRGB

        -- Apply the centering transform so that the output has the zero
        --      frequency in the middle of the image.
        arrCentered     <- computeUnboxedP
                        $  center2d arrComplex
                
        -- Do the 2d transform.
        arrFreq         <- fft2dP Forward arrCentered
                                
        -- Write out the magnitude of the transformed array, 
        --      clipping it at the given value.
        let clip m      = if m >= clipMag then 1 else (m / clipMag)
        arrMag          <- computeUnboxedP 
                        $  R.map (rgb8OfGreyDouble . clip . mag) 
                        $  arrFreq

        writeImageToBMP fileMag arrMag

        -- Write out the phase of the transformed array, 
        --      scaling it to make full use of the 8 bit greyscale.
        let scaledArg x = (arg x + pi) / (2 * pi)
        arrPhase        <- computeUnboxedP 
                        $  R.map (rgb8OfGreyDouble . scaledArg) arrFreq

        writeImageToBMP filePhase arrPhase


