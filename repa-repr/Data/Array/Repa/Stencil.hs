{-# LANGUAGE 	MagicHash, PatternGuards, BangPatterns, TemplateHaskell, QuasiQuotes,
		ParallelListComp, TypeOperators, ExplicitForAll, ScopedTypeVariables #-}
{-# OPTIONS -Wnot #-}

-- | Efficient computation of stencil based convolutions.
--
--   This is specialised for stencils up to 7x7.
--   Due to limitations in the GHC optimiser, using larger stencils doesn't work, and will yield `error`
--   at runtime. We can probably increase the limit if required -- just ask.
--
--   The focus of the stencil is in the center of the 7x7 tile, which has coordinates (0, 0).
--   All coefficients in the stencil must fit in the tile, so they can be given X,Y coordinates up to
--   +/- 3 positions. The stencil can be any shape, and need not be symmetric -- provided it fits in the 7x7 tile.
--
module Data.Array.Repa.Stencil
	( Stencil	(..)
	, Boundary	(..)

	-- * Stencil creation.
	, makeStencil)
where
import Data.Array.Repa
import Data.Array.Repa.Base
import Data.Array.Repa.Stencil.Base
import Data.Array.Repa.Stencil.Template
import Data.Array.Repa.Specialised.Dim2

