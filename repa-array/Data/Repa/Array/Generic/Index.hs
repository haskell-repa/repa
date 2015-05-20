
-- | Shapes and Indices
module Data.Repa.Array.Generic.Index
        ( -- * Shapes
          Shape (..)
        , inShape
        , showShape

          -- ** Polymorphic Shapes
        , Z    (..)
        , (:.) (..)

          -- | Synonyms for common layouts.
        , SH0,  SH1,   SH2,  SH3,  SH4,  SH5

          -- | Helpers that constrain the coordinates to be @Int@s.
        , ish0, ish1, ish2, ish3, ish4, ish5

          -- * Layouts
        , Layout(..),   LayoutI)
where
import Data.Repa.Array.Internals.Shape
import Data.Repa.Array.Internals.Layout
