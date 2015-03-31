
-- | Convert Haskell values to and from a compact binary representation.
--
--   This package is intended for cheap and cheerful serialisation and
--   deserialisation of flat tables, where each row has a fixed format.
--   It is not intended for parsing of general context-free, 
--   or context-sensitive languages. If you want a real parser then try
--   the @parsec@ or @attoparsec@ packages. If you have binary data that
--   does not have a fixed format then try the @binary@ or @cereal@ packages.
--   If you have a table consisting of a couple hundred megs of
--   Pipe-Separated-Variables issued by some filthy enterprise system,
--   then this package is for you.
--
--   For testing purposes, use `packToList` which takes a format, a record,
--   and returns a list of bytes.
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
          packToList
        , unpackFromList

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
        , FixString     (..)
        , VarString     (..)
        , ASCII         (..)

          -- * Atomic values
          -- ** 8-bit
        , Word8be   (..),       Int8be  (..)

          -- ** 16-bit
        , Word16be  (..),       Int16be (..)

          -- ** 32-bit
        , Word32be  (..),       Int32be (..)
        , Float32be (..)

          -- ** 64-bit
        , Word64be  (..),       Int64be (..)
        , Float64be (..))


where
import Data.Repa.Product
import Data.Repa.Convert.Format.Base
import Data.Repa.Convert.Format.Binary
import Data.Repa.Convert.Format.Fields

