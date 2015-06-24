
-- | This module provides the `Format` class definition,
--   without exporting the pre-defined formats.
module Data.Repa.Convert.Format
        ( -- * Packing single fields
          Format   (..)

          -- * Packable
        , Packable  (..)

          -- ** Packer
        , Packer    (..)
        , unsafeRunPacker

          -- ** Unpacker
        , Unpacker  (..)
        , unsafeRunUnpacker)
where
import Data.Repa.Convert.Internal.Format
import Data.Repa.Convert.Internal.Packable
import Data.Repa.Convert.Internal.Packer
import Data.Repa.Convert.Internal.Unpacker
