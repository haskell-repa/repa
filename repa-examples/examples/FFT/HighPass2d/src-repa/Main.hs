{-# LANGUAGE PatternGuards #-}

-- | Perform high pass filtering on a BMP image.
import Data.Array.Repa.Algorithms.FFT           as R
import Data.Array.Repa.Algorithms.DFT.Center    as R
import Data.Array.Repa.Algorithms.Complex       as R
import Data.Array.Repa.IO.BMP                   as R
import Data.Array.Repa.IO.Timing                as R
import Data.Array.Repa                          as R
import qualified Data.Array.Repa.Repr.Unboxed   as U
import System.Environment
import Control.Monad
import Data.Word


main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         [cutoff, fileIn, fileOut]
           -> mainWithArgs (read cutoff) fileIn fileOut

         _ -> putStr $ unlines
                [ "Usage: repa-fft-highpass <cutoff::Int> <fileIn.bmp> <fileOut.bmp>"
                , ""
                , "    Image dimensions must be powers of two, eg 128x512 or 64x256"
                , "" ]
                        
        
mainWithArgs cutoff fileIn fileOut
 = do   
        -- Load in the matrix.
        arrRGB  <- liftM (either (\e -> error $ show e) id)
                $  readImageFromBMP fileIn
        
        let (arrRed, arrGreen, arrBlue)
                = U.unzip3 arrRGB
        
        -- Do the transform on each component individually
        ((arrRed', arrGreen', arrBlue'), t)
                <- time
                $ do    arrRed'         <- transformP cutoff arrRed
                        arrGreen'       <- transformP cutoff arrGreen
                        arrBlue'        <- transformP cutoff arrBlue
                        return  (arrRed', arrGreen', arrBlue')
        
        putStr (prettyTime t)
        
        -- Write it back to file.
        writeImageToBMP fileOut
                (U.zip3 arrRed' arrGreen' arrBlue')
                

-- | Perform high-pass filtering on a rank-2 array.
transformP :: Monad m => Int -> Array U DIM2 Word8 -> m (Array U DIM2 Word8)
transformP cutoff arrReal
 = do   let arrComplex  = R.map (\r -> (fromIntegral r, 0)) arrReal
                        
        -- Do the 2d transform.
        arrCentered     <- computeUnboxedP $ center2d arrComplex
        arrFreq         <- fft2dP Forward arrCentered

        -- Zap out the low frequency components.
        let _ :. height :. width = extent arrFreq
        let centerX              = width  `div` 2
        let centerY              = height `div` 2
        
        let {-# INLINE highpass #-}
            highpass get ix@(_ :. y :. x)
                |   x > centerX + cutoff
                 || x < centerX - cutoff
                 || y > centerY + cutoff
                 || y < centerY - cutoff
                = get ix
                
                | otherwise
                = 0
                
        arrFilt <- computeUnboxedP $ R.traverse arrFreq id highpass

        -- Do the inverse transform to get back to image space.
        arrInv  <- fft2dP Inverse arrFilt
                
        -- Get the magnitude of the transformed array, 
        computeUnboxedP $ R.map (truncate . mag) arrInv

