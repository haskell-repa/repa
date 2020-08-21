{-# OPTIONS -Wnot #-}

-- | Efficient computation of stencil based convolutions.
--
module Data.Array.Repa.Stencil
        ( Stencil       (..)
        , Boundary      (..)

        -- * Stencil creation.
        , makeStencil)
where
import Data.Array.Repa
import Data.Array.Repa.Base
import Data.Array.Repa.Stencil.Base
import Data.Array.Repa.Specialised.Dim2

