
module Data.Array.Repa.Stencil.Partition
        ( Offset (..)
        , Size   (..)
        , Region (..)
        , partitionForStencil)
where

-- | An offset in the 2d plane.
data Offset
        = Offset !Int !Int

-- | Size of a region in the 2d plane.
data Size
        = Size   !Int !Int

-- | A region in the 2d plane.
data Region 
        = Region
        { regionX       :: !Int
        , regionY       :: !Int
        , regionWidth   :: !Int
        , regionHeight  :: !Int }
        deriving Show


-- | Create a new region of the given size.
regionOfSize :: Size -> Region
regionOfSize (Size w h)
        = Region 0 0 w h
{-# INLINE regionOfSize #-}

-- | Offset a region.
offsetRegion :: Offset -> Region -> Region
offsetRegion (Offset x y) (Region x0 y0 w h)
        = Region (x0 + x) (y0 + y) w h
{-# INLINE offsetRegion #-}

-- | Partition a region into inner and border regions for the given stencil.
partitionForStencil
        :: Size         -- ^ Size of array
        -> Size         -- ^ Size of stencil
        -> Offset       -- ^ Focus of stencil
        -> [Region]

partitionForStencil
          (Size   arrW arrH)
          (Size   krnW krnH)
          (Offset focX focY)
 = let  
        gapNorth        = focY
        gapSouth        = krnH - focY - 1
        gapWest         = focX
        gapEast         = krnW - focX - 1

        innerW          = arrW - gapWest  - gapEast
        innerH          = arrH - gapNorth - gapSouth

        regionInner     = offsetRegion (Offset  gapWest           gapNorth)
                        $ regionOfSize (Size    innerW            innerH)

        regionNorth     = regionOfSize (Size    arrW              gapNorth)

        regionSouth     = offsetRegion (Offset  0                 (gapNorth + innerH))
                        $ regionOfSize (Size    arrW              gapSouth)

        regionWest      = offsetRegion (Offset  0                 gapNorth)
                        $ regionOfSize (Size    gapWest           innerH)

        regionEast      = offsetRegion (Offset (gapWest + innerW) gapNorth)
                        $ regionOfSize (Size    gapEast           innerH)

  in    [regionInner, regionNorth, regionSouth, regionWest, regionEast]
{-# INLINE partitionForStencil #-}
