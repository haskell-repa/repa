
-- | Convert tuples of Haskell values to and from packed binary or
--   ASCII representations.
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
--   For testing purposes, use `packToList` which takes a format,
--   a record, and returns a list of bytes.
--
-- @
-- > import Data.Repa.Convert.Format
--
-- > let Just bytes = packToList (FixString ASCII 10 :*: Word16be :*: Float64be) ("foo" :*: 66 :*: 93.42)
-- > bytes
-- [102,111,111,0,0,0,0,0,0,0,0,66,64,87,90,225,71,174,20,123]
-- @
--
-- We can then unpack the raw bytes back to Haskell values with `unpackFromList`.
--
-- @
-- > unpackFromList (FixString ASCII 10 :*: Word16be :*: Float64be) bytes 
-- Just ("foo" :*: (66 :*: 93.42))
-- @
--
-- In production code use `pack` and `unpack` to work directly with a buffer in foreign memory.
--
module Data.Repa.Convert.Format
        ( -- * Conversion
          packToList,   unpackFromList
        , packToString, unpackFromString

          -- * Data formats
        , Format   (..)

          -- * Packing records
        , Packable  (..)
        , Packables (..)

          -- * Constraints
        , forFormat
        , listFormat

          -- * Field Products
        , (:*:)(..)

          -- * Field Separators
        , App (..)
        , Sep (..)

          -- * Lists
        , FixList(..)
        , VarList(..)

          -- * Strings
        , FixAsc (..)
        , VarAsc (..)

          -- * Atomic values
          -- ** ASCII numeric
        , IntAsc     (..)
        , DoubleAsc  (..)

          -- ** ASCII dates
        , YYYYsMMsDD (..)
        , DDsMMsYYYY (..)

          -- ** 8-bit binary
        , Word8be    (..),       Int8be  (..)

          -- ** 16-bit binary
        , Word16be   (..),       Int16be (..)

          -- ** 32-bit binary
        , Word32be   (..),       Int32be (..)
        , Float32be  (..)

          -- ** 64-bit binary
        , Word64be   (..),       Int64be (..)
        , Float64be  (..))
where
import Data.Repa.Product
import Data.Repa.Convert.Format.Base
import Data.Repa.Convert.Format.Binary
import Data.Repa.Convert.Format.Lists
import Data.Repa.Convert.Format.Fields
import Data.Repa.Convert.Format.Numeric
import Data.Repa.Convert.Format.Date32

