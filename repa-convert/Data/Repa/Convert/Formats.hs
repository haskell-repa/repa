
-- | Pre-defined data formats.
module Data.Repa.Convert.Formats
        ( -- * Default
          FormatAscii(..)
        
        -- * Units
        , UnitAsc       (..)

          -- * Maybes
        , MaybeChars    (..)
        , MaybeBytes    (..)

          -- * String Formats
          -- ** for Haskell Strings
        , FixChars      (..)
        , VarChars      (..)
        , VarCharString (..)
        , ExactChars    (..)

          -- ** for Data.Text
        , VarText       (..)
        , VarTextString (..)

          -- ** for Data.ByteString
        , VarBytes      (..)

          -- * ASCII Atoms
          -- ** ASCII integers
        , IntAsc        (..)
        , IntAsc0       (..)

          -- ** ASCII doubles
        , DoubleAsc     (..)
        , DoubleFixedPack (..)

          -- ** ASCII dates
        , YYYYsMMsDD    (..)
        , DDsMMsYYYY    (..)

          -- * Binary Atoms
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
        , Float64be     (..)

          -- * Compounds
          -- ** Appended fields
        , App                   (..)

          -- ** Tuples
        , Tup                   (..)

          -- ** Separated fields
        , Sep,  SepFormat       (..)

          -- ** Object with labeled fields
        , Object, ObjectFormat, Field (..), mkObject

          -- * Products
        , (:*:)(..))
where
import Data.Repa.Convert.Format.Ascii
import Data.Repa.Convert.Format.App
import Data.Repa.Convert.Format.Binary
import Data.Repa.Convert.Format.Bytes
import Data.Repa.Convert.Format.Date32
import Data.Repa.Convert.Format.Fields  ()
import Data.Repa.Convert.Format.Maybe
import Data.Repa.Convert.Format.Numeric
import Data.Repa.Convert.Format.Object
import Data.Repa.Convert.Format.Sep
import Data.Repa.Convert.Format.String
import Data.Repa.Convert.Format.Text
import Data.Repa.Convert.Format.Tup
import Data.Repa.Convert.Format.Unit
import Data.Repa.Scalar.Product

