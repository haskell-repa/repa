
-- | This module provides the `Format` class definition,
--   without exporting the pre-defined formats.
module Data.Repa.Convert.Format
        ( -- * Packing single fields
          Format   (..)

          -- * Packing records
        , Packable  (..))
where
import Data.Repa.Convert.Format.Base
