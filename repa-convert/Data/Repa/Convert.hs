
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
-- > let format   = Sep '|' (VarAsc :*: IntAsc :*: DoubleAsc :*: ())
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

          -- * Packing data
        , Packable  (..)

          -- ** Packer monoid
        , Packer (..)
        , unsafeRunPacker

          -- ** Unpacker monad
        , Unpacker (..)
        , unsafeRunUnpacker

          -- * Interfaces
          -- ** Default Ascii
        , packToAscii

          -- ** List Interface
        , packToList8,  unpackFromList8

          -- ** String Interface
        , packToString, unpackFromString)
where
import Data.Repa.Convert.Format
import Data.Repa.Convert.Formats
import Data.Char
import Data.Word
import Control.Monad
import System.IO.Unsafe
import qualified Foreign.Storable               as S
import qualified Foreign.Marshal.Alloc          as S
import qualified GHC.Ptr                        as S


---------------------------------------------------------------------------------------------------
-- | Pack a value to a list of `Word8` using the default Ascii format.
packToAscii
        :: ( FormatAscii a
           , Value    (FormatAscii' a) ~ a
           , Packable (FormatAscii' a))
        => a -> Maybe String
packToAscii a
        = packToString (formatAscii a) a


---------------------------------------------------------------------------------------------------
-- | Pack a value to a list of `Word8`.
packToList8 
        :: Packable format
        => format -> Value format -> Maybe [Word8]
packToList8 f x
 | Just lenMax  <- packedSize f x
 = unsafePerformIO
 $ do   buf     <- S.mallocBytes lenMax
        mResult <- unsafeRunPacker (pack f x) buf
        case mResult of
         Nothing      -> return Nothing
         Just buf' 
          -> do let lenUsed = S.minusPtr buf' buf
                xs      <- mapM (S.peekByteOff buf) [0 .. lenUsed - 1]
                S.free buf
                return $ Just xs

 | otherwise    = Nothing


-- | Unpack a value from a list of `Word8`.
unpackFromList8
        :: Packable format
        => format -> [Word8] -> Maybe (Value format)

unpackFromList8 format xs
 = unsafePerformIO
 $ do   let len = length xs
        buf     <- S.mallocBytes len
        mapM_ (\(o, x) -> S.pokeByteOff buf o x)
                $ zip [0 .. len - 1] xs
        r <- unsafeRunUnpacker (unpack format) buf len (const False)
        return $ fmap fst r


-- | Pack a value to a String.
packToString
        :: Packable format
        => format -> Value format -> Maybe String
packToString f v
        = liftM (map (chr . fromIntegral)) $ packToList8 f v


-- | Unpack a value from a String.
unpackFromString 
        :: Packable format
        => format -> String -> Maybe (Value format)
unpackFromString f s
        = unpackFromList8 f $ map (fromIntegral . ord) s


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
