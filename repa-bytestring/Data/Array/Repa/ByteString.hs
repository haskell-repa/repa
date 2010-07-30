{-# LANGUAGE ScopedTypeVariables, PackageImports #-}

-- | Conversions between Repa Arrays and ByteStrings.
module Data.Array.Repa.ByteString
	( toByteString
	, fromByteString)
where
import Data.Word
import Data.Array.Repa
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import qualified Data.ByteString	as BS
import Data.ByteString			(ByteString)
import qualified "dph-prim-par" Data.Array.Parallel.Unlifted as U


-- | Convert an `Array` to a (strict) `ByteString`.
toByteString 
	:: Shape sh
	=> Array sh Word8
	-> ByteString

toByteString arr
 = unsafePerformIO
 $ allocaBytes (size $ extent arr)	$ \(bufDest :: Ptr Word8) ->
   let 	uarr	= toUArray arr
	len	= size $ extent arr

	copy offset
	 | offset >= len
	 = return ()

	 | otherwise
	 = do	pokeByteOff bufDest offset (uarr U.!: offset)
		copy (offset + 1)

    in do
	copy 0
	BS.packCStringLen (castPtr bufDest, len)


-- | Convert a (strict) `ByteString` to an `Array`.
--	The given array size must match the length of the `ByteString`, else `error`.
fromByteString 
	:: Shape sh
	=> sh
	-> ByteString 
	-> Array sh Word8

-- TODO: Use an intermediate CString when we do this, to avoid
--	 the overhead from bounds checks in ByteString
fromByteString sh str
	| size sh /= BS.length str
	= error "Data.Array.Repa.fromByteString: given array size does not match length of string"
	
	| otherwise
	= fromFunction sh (\ix -> str `BS.index` toIndex sh ix)

