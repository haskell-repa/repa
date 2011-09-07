
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Word
import Data.Bits
import Data.Array.Repa                  as R
import Data.Array.Repa.IO.Binary        as R
import Data.Array.Repa.IO.BMP           as R
import Data.Array.Repa.IO.ColorRamp     as R
import Prelude                          as P
import System.Environment
import Control.Monad

-- | Cuts slices out of a volume cube of Word16 data.
main :: IO ()
main 
 = do   args <- getArgs
        case args of
         [fileIn, fileOut, depth', height', width', sliceNum', low', high']
          ->    run fileIn fileOut
                        (read depth')    (read height') (read width') 
                        (read sliceNum') (read low')    (read high')

         _ -> do
                putStr  $ unlines
                        [ "usage: volume <fileIn> <fileOut> <depth> <height> <width> <sliceNum> <lowVal> <highVal>" ]

run :: FilePath -> FilePath -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
run fileIn fileOut depth width height sliceNum low high
 = do   
        -- Read data from the raw file of Word16s.
        let arraySize   = (Z :. depth :. width  :. height)
        (arr :: Array DIM3 Word16) 
         <- R.readArrayFromStorableFile fileIn arraySize

        -- Ensure it's all read in before proceeding.
        arr `deepSeqArray` return ()
        dumpSlice fileOut arr sliceNum low high 


-- | Dump a numbered slice of this array to a BMP file.
dumpSlice 
	:: FilePath             -- output base name
	-> Array DIM3 Word16    -- source data
	-> Int                  -- array slice number
	-> Int                  -- low value for color ramp
	-> Int                  -- high value for color ramp
	-> IO ()

dumpSlice fileBase arr sliceNum low high
 = do	-- slice out the part that we want from the cube 
        let arrSlice	= slice arr (Any :. sliceNum :. All :. All)

        -- select a part of the large dynamic range
	let arrBrack    :: Array DIM2 Word16
	    arrBrack	= R.map (bracket low high . fromIntegral . flip16) arrSlice

        -- invert the y coordinate so the image is the correct way around
        let (Z :. height :. _) = R.extent arrSlice
        let arrInv      = R.traverse arrBrack id 
                                (\get (Z :. y :. x) -> get (Z :. (height - 1) - y :. x))

        -- dump the slice back as word16
	R.writeArrayToStorableFile (fileBase P.++ ".w16") arrInv

        -- colorise and write to BMP file
        let arrColor :: Array DIM2 (Double, Double, Double)
            arrColor    = R.map (\x -> if x == 0
                                        then (0, 0, 0)
                                        else rampColorHotToCold 0 255 x)
                        $ R.map fromIntegral arrInv
        
        let arrColor'   = R.force
                        $ R.map (\(r, g, b) ->  ( truncate (r * 255)
                                                , truncate (g * 255)
                                                , truncate (b * 255)))
                        $ arrColor

        R.writeComponentsToBMP (fileBase P.++ ".bmp")
                (R.map (\(r, g, b) -> r) arrColor')
                (R.map (\(r, g, b) -> g) arrColor')
                (R.map (\(r, g, b) -> b) arrColor') 


{-# INLINE bracket #-}
bracket low high x
        | x < low      = 0
        | x > high     = 255
        | otherwise    
        = let  r       = fromIntegral (x - low) / fromIntegral (high - low)
          in   truncate (r * 255)


{-# INLINE flip16 #-}
flip16 :: Word16 -> Word16
flip16 xx
        = shift xx 8 .|. (shift xx (-8) .&. 0x00ff)

