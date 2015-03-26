
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

          -- * ASCII Strings
        , FixString     (..)
        , VarString     (..)
        , ASCII         (..)

          -- * Atomic values
        , Word8be   (..),       Int8be  (..)
        , Word16be  (..),       Int16be (..)
        , Word32be  (..),       Int32be (..)
        , Word64be  (..),       Int64be (..)

        , Float32be (..)
        , Float64be (..))

where
import Data.Repa.Product
import Data.Repa.Convert.Format.Base
import Data.Repa.Convert.Format.Binary


