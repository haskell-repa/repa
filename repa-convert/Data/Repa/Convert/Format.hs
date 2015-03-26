
-- | Convert Haskell values to and from a compact binary representation.
-- 
--  For testing purposes, use `packToList` which takes a format, a record, and returns a list of bytes.
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
        ( -- * Data formats
          Format   (..)

          -- * Packing records
        , Packable (..)
        , packToList
        , unpackFromList

          -- * Products
        , (:*:)(..)

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


