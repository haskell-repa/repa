
-- | Pre-defined data formats.
module Data.Repa.Convert.Formats
        ( -- * Field Products
          (:*:)(..)

          -- * Field Separators
        , App           (..)
        , Sep           (..)
        , Row           (..)

          -- * Lists
        , FixList       (..)
        , VarList       (..)

          -- * Strings
        , FixAsc        (..)
        , VarAsc        (..)
        , VarString     (..)

          -- * Atomic values
          -- ** ASCII numeric
        , IntAsc        (..)
        , IntAsc0       (..)
        , DoubleAsc     (..)

          -- ** ASCII dates
        , YYYYsMMsDD    (..)
        , DDsMMsYYYY    (..)

          -- ** 8-bit binary
        , Word8be       (..)
        , Int8be        (..)

          -- ** 16-bit binary
        , Word16be      (..)
        , Int16be       (..)

          -- ** 32-bit binary
        , Word32be      (..)
        , Int32be       (..)
        , Float32be     (..)

          -- ** 64-bit binary
        , Word64be      (..)
        , Int64be       (..)
        , Float64be     (..))
where
import Data.Repa.Convert.Format.App
import Data.Repa.Convert.Format.Binary
import Data.Repa.Convert.Format.Date32
import Data.Repa.Convert.Format.Fields  ()
import Data.Repa.Convert.Format.Lists
import Data.Repa.Convert.Format.Numeric
import Data.Repa.Convert.Format.Row
import Data.Repa.Convert.Format.Sep
import Data.Repa.Product

