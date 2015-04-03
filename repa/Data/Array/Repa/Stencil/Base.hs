
-- | Basic definitions for stencil handling.
module Data.Array.Repa.Stencil.Base
        ( Boundary      (..)
        , Stencil       (..)
        , makeStencil, makeStencil2)
where
import Data.Array.Repa.Index

-- | How to handle the case when the stencil lies partly outside the array.
data Boundary a
        -- | Use a fixed value for border regions.
        = BoundFixed !a

        -- | Treat points outside the array as having a constant value.
        | BoundConst !a

        -- | Clamp points outside to the same value as the edge pixel.
        | BoundClamp
        deriving (Show)


-- | Represents a convolution stencil that we can apply to array.
--   Only statically known stencils are supported right now.
data Stencil sh a

        -- | Static stencils are used when the coefficients are fixed,
        --   and known at compile time.
        = StencilStatic
        { stencilExtent :: !sh
        , stencilZero   :: !a
        , stencilAcc    :: !(sh -> a -> a -> a) }


-- | Make a stencil from a function yielding coefficients at each index.
makeStencil
        :: Num a
        => sh                   -- ^ Extent of stencil.
        -> (sh -> Maybe a)      -- ^ Get the coefficient at this index.
        -> Stencil sh a

{-# INLINE makeStencil #-}
makeStencil ex getCoeff
 = StencilStatic ex 0
 $ \ix val acc
        -> case getCoeff ix of
                Nothing         -> acc
                Just coeff      -> acc + val * coeff


-- | Wrapper for `makeStencil` that requires a DIM2 stencil.
makeStencil2
        :: Num a
        => Int -> Int           -- ^ extent of stencil
        -> (DIM2 -> Maybe a)    -- ^ Get the coefficient at this index.
        -> Stencil DIM2 a

{-# INLINE makeStencil2 #-}
makeStencil2 height width getCoeff
        = makeStencil (Z :. height :. width) getCoeff

