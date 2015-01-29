
-- | Shapes and Indices
module Data.Repa.Array.Index
	( -- * Shapes
          Shape (..)
        , inShape
        , showShape

          -- ** Polymorphic Shapes
        , Z    (..)
        , (:.) (..)
        , SH0, SH1, SH2, SH3, SH4, SH5

          -- * Layouts
        , Layout(..)

          -- ** RowWise Layout
        , RowWise (..)
        , DIM1, DIM2, DIM3, DIM4, DIM5
        , ix1,  ix2,  ix3,  ix4,  ix5)
where
import Data.Repa.Array.Internals.Shape
import Data.Repa.Array.Internals.Layout
import Data.Repa.Array.Internals.RowWise
