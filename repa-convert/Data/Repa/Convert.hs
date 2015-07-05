
-- | Convert tuples of Haskell values to and from ASCII or packed binary
--   representations.
--
--   This package is intended for cheap and cheerful serialisation and
--   deserialisation of flat tables, where each row has a fixed format.
--   If you have a table consisting of a couple hundred megs of
--   Pipe-Separated-Variables issued by some filthy enterprise system,
--   then this package is for you.
--
--   If you want to parse context-free, or context-sensitive
--   languages then try the @parsec@ or @attoparsec@ packages.
--   If you have binary data that does not have a fixed format then
--   try the @binary@ or @cereal@ packages.
--
--   For testing purposes, use `packToString` which takes a format,
--   a record, and returns a list of bytes.
--
-- @
-- > import Data.Repa.Convert
--
-- > let format   = mkSep '|' (VarChars :*: IntAsc :*: DoubleAsc :*: ())
-- > let Just str = packToString format ("foo" :*: 66 :*: 93.42 :*: ())
-- > str
-- "foo|66|93.42"
-- @
--
-- We can then unpack the raw bytes back to Haskell values with `unpackFromString`.
--
-- @
-- > unpackFromString format str 
-- Just ("foo" :*: (66 :*: (93.42 :*: ())))
-- @
--
-- In production code use `unsafeRunPacker` and `unsafeRunUnpacker` to work directly
-- with a buffer in foreign memory.
--
-- * NOTE that in the current version the separating character is un-escapable. 
-- * The above means that the format @(Sep ',')@ does NOT parse a CSV
--   file according to the CSV specification: http://tools.ietf.org/html/rfc4180.
--
module Data.Repa.Convert
        ( -- | The @Formats@ module contains the pre-defined data formats.
          module Data.Repa.Convert.Formats

          -- * Data formats  
        , Format    (..)

          -- * Type constraints
        , forFormat
        , listFormat

          -- * High-level interface
          -- ** for ByteStrings
        , packToByteString
        , unpackFromByteString

          -- ** for Lists of Word8
        , packToList8
        , unpackFromList8

          -- ** for Strings
        , packToString
        , unpackFromString

          -- * Low-level interface
          -- ** Packing data
        , Packable  (..)

          -- ** Packer monoid
        , Packer (..)
        , unsafeRunPacker

          -- ** Unpacker monad
        , Unpacker (..)
        , unsafeRunUnpacker)
where
import Data.Repa.Convert.Format
import Data.Repa.Convert.Formats
import Data.Word
import System.IO.Unsafe
import Data.IORef
import Data.ByteString                          (ByteString)
import qualified Data.ByteString.Internal       as BS
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BS8
import qualified Foreign.ForeignPtr             as F
import qualified Foreign.Marshal.Alloc          as F
import qualified Foreign.Marshal.Utils          as F
import qualified GHC.Ptr                        as F
#include "repa-convert.h"


---------------------------------------------------------------------------------------------------
-- | Constrain the type of a value to match the given format.
-- 
--   The value itself is not used.
--
forFormat :: format -> Value format  -> Value format
forFormat _ v = v
{-# INLINE forFormat #-}


-- | Constrain the type of some values to match the given format.
--
--   The value itself is not used.
--
listFormat :: format -> [Value format] -> [Value format]
listFormat _ v = v
{-# INLINE listFormat #-}


---------------------------------------------------------------------------------------------------
-- | Pack a value to a freshly allocated `ByteString`.
packToByteString
        :: Packable format
        => format -> Value format -> Maybe ByteString

packToByteString format value

 -- The size returned by `packedSize` is an over-approximation.
 --   As we don't want to waste space in the returned value, 
 --   we pack the value to a stack allocated buffer, 
 --   then copy it into a newly allocated ByteString when we know
 --   how much space we actually need.
 |  Just lenMax <- packedSize format value
 =  unsafePerformIO
 $  F.allocaBytes lenMax $ \buf
 -> do
        -- Pack the value into the on-stack buffer.
        let !(F.Ptr addr) = buf
        !ref    <- newIORef Nothing
        packer format value addr
                (return ())
                (\buf' -> writeIORef ref (Just (F.Ptr buf')))
        mEnd    <- readIORef ref

        -- See if the packer worked.
        case mEnd of
         Just end
          -> do -- Now work out how much space we actually used.
                let !lenPacked = F.minusPtr end buf

                -- Allocate a new buffer of the right size, 
                -- and copy the data into it.
                F.mallocForeignPtrBytes lenPacked >>= \fptr
                 -> F.withForeignPtr fptr $ \ptr
                 -> do  F.copyBytes ptr buf lenPacked
                        return $ Just $ BS.PS fptr 0 lenPacked

         Nothing
          -> return Nothing

 | otherwise
 = Nothing
{-# INLINE packToByteString #-}


-- | Unpack a value from a `ByteString`.
unpackFromByteString
        :: Packable format
        => format -> ByteString -> Maybe (Value format)

unpackFromByteString format (BS.PS fptr offset len)
 -- If the bytestring is too short to hold a value of the minimum
 -- size then we're going to have a bad time.
 | len < minSize format
 = Nothing

 -- Open up the bytestring and try to unpack its contents.
 | otherwise
 = unsafePerformIO
 $ F.withForeignPtr fptr $ \ptr_
 -> do  
        let !(F.Ptr start) = F.plusPtr ptr_  offset
        let !(F.Ptr end)   = F.plusPtr (F.Ptr start) len

        !ref    <- newIORef Nothing
        unpacker format start end 
                (const False)
                (return ())
                (\done' value -> writeIORef ref $ Just (F.Ptr done', value))
        mResult <- readIORef ref

        return $ case mResult of
         Nothing              -> Nothing
         Just (done, value)
          | done /= F.Ptr end -> Nothing
          | otherwise         -> Just value
{-# INLINE unpackFromByteString #-}


---------------------------------------------------------------------------------------------------
-- | Pack a value to a list of `Word8`.
packToList8 
        :: Packable format
        => format -> Value format -> Maybe [Word8]
packToList8 format value
 = fmap BS.unpack $ packToByteString format value
{-# INLINE packToList8 #-}


-- | Unpack a value from a list of `Word8`.
unpackFromList8
        :: Packable format
        => format -> [Word8] -> Maybe (Value format)

unpackFromList8 format ws
 = unpackFromByteString format $ BS.pack ws
{-# INLINE unpackFromList8 #-}


---------------------------------------------------------------------------------------------------
-- | Pack a value to a (hated) Haskell `String`.
packToString
        :: Packable format
        => format -> Value format -> Maybe String
packToString format value
 = fmap BS8.unpack $ packToByteString format value
{-# INLINE packToString #-}


-- | Unpack a value from a (hated) Haskell `String`.
unpackFromString 
        :: Packable format
        => format -> String -> Maybe (Value format)
unpackFromString format ss
 = unpackFromByteString format $ BS8.pack ss
{-# INLINE unpackFromString #-}

