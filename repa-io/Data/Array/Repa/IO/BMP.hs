{-# LANGUAGE PackageImports, PatternGuards, ExplicitForAll  #-} 

-- | Reading and writing arrays as uncompressed 24 or 32 bit Windows BMP files.
module Data.Array.Repa.IO.BMP
	( readImageFromBMP
        , writeImageToBMP)
where
import Data.Array.Repa				as R
import Data.Array.Repa.Unsafe                   as R
import Data.Array.Repa.Repr.ForeignPtr		as R
import Data.Array.Repa.Repr.ByteString		as R
import Data.Vector.Unboxed                      as U
import Prelude					as P
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Data.ByteString.Unsafe                   as B
import Codec.BMP
import Data.Word

-- NOTE: We set most of these functions as NOINLINE so it's easier to understand
--       what's going on in the core programs. The top-level IO functions are
--       only called a few times each, so it doesn't matter if they're not
--       worker/wrappered etc.
	
-- Read -------------------------------------------------------------------------------------------
-- | Read RGB components from a BMP file.
readImageFromBMP
	:: FilePath
	-> IO (Either Error (Array U DIM2 (Word8, Word8, Word8)))

{-# NOINLINE readImageFromBMP #-}
readImageFromBMP filePath
 = do   ebmp	<- readBMP filePath

	case ebmp of
	 Left err	-> return $ Left err
	 Right bmp	
	  -> do arr     <- readImageFromBMP' bmp
	        return  $ Right arr

readImageFromBMP' bmp
 = do	let (width, height) = bmpDimensions bmp

	let arr		= R.fromByteString (Z :. height :. width * 4)
			$ unpackBMPToRGBA32 bmp

	let shapeFn _ 	= Z :. height :. width

        -- Read out the components into their own arrays, 
        -- skipping the alpha channel.
	vecRed         <- computeP 
                        $ unsafeTraverse arr shapeFn
                	       (\get (sh :. x) -> get (sh :. (x * 4)))

	vecGreen       <- computeP 
                        $ unsafeTraverse arr shapeFn
                		(\get (sh :. x) -> get (sh :. (x * 4 + 1)))

	vecBlue        <- computeP
                        $ unsafeTraverse arr shapeFn
                		(\get (sh :. x) -> get (sh :. (x * 4 + 2)))

	-- O(1). zip the components together
	let vecRGB     = U.zip3 (toUnboxed vecRed)
                                (toUnboxed vecGreen)
                                (toUnboxed vecBlue)
	
        return $ fromUnboxed (Z :. height :. width) vecRGB



-- | Write RGB components to a BMP file.
writeImageToBMP
	:: FilePath
	-> Array U DIM2 (Word8, Word8, Word8)
	-> IO ()

{-# NOINLINE writeImageToBMP #-}
writeImageToBMP fileName arrRGB
 = do   let sh@(Z :. height :. width)
                        = extent arrRGB

        -- O(1). unzip the components
	let (vecRed, vecGreen, vecBlue)
	        = U.unzip3 $ toUnboxed arrRGB


        -- Create a bytestring with all the data
        ptr     <- mallocBytes (height * width * 4)
        fptr    <- newForeignPtr finalizerFree ptr

        computeIntoP fptr 
         $ interleave4 
	        (fromUnboxed sh vecRed)
	        (fromUnboxed sh vecGreen)
	        (fromUnboxed sh vecBlue)
	        (fromFunction sh (\_ -> 255))

        -- Pack the data into a BMP file and write it out.
        withForeignPtr fptr
         $ \ptr' -> do
                bs      <- unsafePackCStringFinalizer ptr' (width * height * 4) (return ())
                let bmp = packRGBA32ToBMP width height bs
                writeBMP fileName bmp


{-
-- Normalise --------------------------------------------------------------------------------------
-- | Normalise a matrix to to [0 .. 1], discarding negative values.
--	If the maximum value is 0 then return the array unchanged.
normalisePositive01
	:: (Shape sh, Fractional a, Ord a)
	=> Array sh a
	-> Array sh a

{-# INLINE normalisePositive01 #-}
normalisePositive01 arr	
 = let	mx		= foldAll max 0 arr
   	elemFn x
	 | x >= 0	= x / mx
	 | otherwise	= x
   in	mx `seq`
	 if mx == 0 
	  then arr
	  else R.map elemFn arr

-}
