{-# LANGUAGE PatternGuards #-}

import Data.Array.Repa.Algorithms.FFT           as R
import Data.Array.Repa.Algorithms.DFT.Center    as R
import Data.Array.Repa.Algorithms.Complex       as R
import Data.Array.Repa.Algorithms.ColorRamp     as R
import Data.Array.Repa.IO.BMP                   as R
import Data.Array.Repa.IO.Timing                as R
import Data.Array.Repa                          as R
import Prelude                                  as P
import qualified Data.Array.Repa.Repr.Unboxed   as U
import Data.Word
import System.Environment
import Control.Monad


main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         [size, prefix] -> mainWithArgs (read size) prefix

         _ -> putStr $ unlines
                [ "Usage: repa-fft3d-highpass <size::Int> <prefix>"
                , ""
                , "   Size must be a power of two."
                , "   You get a stack of prefix###.bmp files resulting from high-pass filtering a cube."
                , "" ]
                        
                        
mainWithArgs size prefixOut
 = do
        -- Generate a cube for initial data.
        let shape       = Z :. size :. size :. size
        let cubeSize    = size `div` 4
        let center      = size `div` 2
        let cutoff              = 4

        arrInit <- computeP
                $  fromFunction shape 
                        (\ix -> if isInCenteredCube center cubeSize ix 
                                        then (1, 0) else (0, 0))

        (arrFinal, t) <- time $ transformP arrInit center cutoff
        putStr (prettyTime t)

        mapM_ (dumpSlice prefixOut arrFinal) [0..size - 1]


-- | To the high pass transform.
transformP
        :: Monad m
        => Array U DIM3 Complex
        -> Int
        -> Int
        -> m (Array U DIM3 Complex)

transformP arrInit center cutoff
 = do   -- Transform to frequency space.
        let arrCentered = center3d arrInit
        arrFreq         <- fft3dP Forward arrCentered
        
        -- Zap out the high frequency components
        let arrFilt     = R.traverse arrFreq id (highpass center cutoff)
        
        -- Do the inverse transform to get back to image space.
        fft3dP Inverse arrFilt


-- | Dump a numbered slice of this array to a BMP file.
dumpSlice 
        :: FilePath
        -> Array U DIM3 Complex
        -> Int
        -> IO ()

dumpSlice prefix arr sliceNum
 = do   let arrSlice    = slice arr (Any :. sliceNum :. All)
        arrGrey         <- computeUnboxedP $ R.map (truncate . (* 255) . mag) arrSlice
        let fileName    = prefix P.++ (pad0 3 (show sliceNum)) P.++ ".bmp"

        writeImageToBMP fileName
                (U.zip3 arrGrey arrGrey arrGrey)

pad0 n str
 = P.replicate  (n - length str) '0' P.++ str


{-# INLINE isInCenteredCube #-}
isInCenteredCube center cutoff ix@(_ :. z :. y :. x)
 = let  high    = center + cutoff
        low     = center - cutoff
   in   x >= low && x <= high
     && y >= low && y <= high
     && z >= low && z <= high


{-# INLINE highpass #-}
highpass center cutoff get ix
        | isInCenteredCube center cutoff ix     = 0
        | otherwise                             = get ix

