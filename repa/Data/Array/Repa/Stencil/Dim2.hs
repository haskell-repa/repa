{-# LANGUAGE MagicHash #-}
--   This is specialised for stencils up to 7x7.
--   Due to limitations in the GHC optimiser, using larger stencils doesn't
--   work, and will yield `error` at runtime. We can probably increase the
--   limit if required -- just ask.
--
--   The focus of the stencil is in the center of the 7x7 tile, which has
--   coordinates (0, 0). All coefficients in the stencil must fit in the tile,
--   so they can be given X,Y coordinates up to +/- 3 positions.
--   The stencil can be any shape, and need not be symmetric -- provided it
--   fits in the 7x7 tile.
--
module Data.Array.Repa.Stencil.Dim2
        ( -- * Stencil creation
          makeStencil2, stencil2

          -- * Stencil operators
        , PC5, mapStencil2, forStencil2)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.Cursored
import Data.Array.Repa.Repr.Partitioned
import Data.Array.Repa.Repr.HintSmall
import Data.Array.Repa.Repr.Undefined
import Data.Array.Repa.Stencil.Base
import Data.Array.Repa.Stencil.Template
import Data.Array.Repa.Stencil.Partition
import GHC.Exts

-- | A index into the flat array.
--   Should be abstract outside the stencil modules.
data Cursor
        = Cursor Int

type PC5 = P C (P (S D) (P (S D) (P (S D) (P (S D) X))))


-- Wrappers -------------------------------------------------------------------
-- | Like `mapStencil2` but with the parameters flipped.
forStencil2
        :: Source r a
        => Boundary a
        -> Array  r DIM2 a
        -> Stencil  DIM2 a
        -> Array PC5 DIM2 a

{-# INLINE forStencil2 #-}
forStencil2 boundary arr stencil
        = mapStencil2 boundary stencil arr


-------------------------------------------------------------------------------
-- | Apply a stencil to every element of a 2D array.
mapStencil2
        :: Source r a
        => Boundary a           -- ^ How to handle the boundary of the array.
        -> Stencil DIM2 a       -- ^ Stencil to apply.
        -> Array r DIM2 a               -- ^ Array to apply stencil to.
        -> Array PC5 DIM2 a

{-# INLINE mapStencil2 #-}
mapStencil2 boundary stencil@(StencilStatic sExtent _zero _load) arr
 = let  sh                       = extent arr
        (_ :. aHeight :. aWidth) = sh
        (_ :. sHeight :. sWidth) = sExtent

        sHeight2        = sHeight `div` 2
        sWidth2         = sWidth  `div` 2

        -- Partition the array into the internal and border regions.
        ![ Region    inX    inY    inW    inH
         , Region  westX  westY  westW  westH
         , Region  eastX  eastY  eastW  eastH
         , Region northX northY northW northH 
         , Region southX southY southW southH ] 
           = partitionForStencil 
                (Size   aWidth   aHeight) 
                (Size   sWidth   sHeight)
                (Offset sWidth2  sHeight2)

        {-# INLINE inInternal #-}
        inInternal (Z :. y :. x)
                =  x >= inX && x < (inX + inW)
                && y >= inY && y < (inY + inH)

        {-# INLINE inBorder #-}
        inBorder       = not . inInternal

        -- Cursor functions ----------------
        {-# INLINE makec #-}
        makec (Z :. y :. x)
         = Cursor (x + y * aWidth)

        {-# INLINE shiftc #-}
        shiftc ix (Cursor off)
         = Cursor
         $ case ix of
                Z :. y :. x     -> off + y * aWidth + x

        {-# INLINE arrInternal #-}
        arrInternal     = makeCursored (extent arr) makec shiftc getInner' 

        {-# INLINE getInner' #-}
        getInner' cur   = unsafeAppStencilCursor2 shiftc stencil arr cur

        {-# INLINE arrBorder #-}
        arrBorder       = ASmall (fromFunction (extent arr) getBorder')

        {-# INLINE getBorder' #-}
        getBorder' ix
         = case boundary of
                BoundFixed c    -> c
                BoundConst c    -> unsafeAppStencilCursor2_const addDim stencil c arr ix
                BoundClamp      -> unsafeAppStencilCursor2_clamp addDim stencil arr ix
   in
    --  internal region
        APart sh (Range (Z :.    inY :.    inX) (Z :.    inH :.    inW) inInternal) arrInternal

    --  border regions
    $   APart sh (Range (Z :.  westY :.  westX) (Z :.  westH :.  westW) inBorder)   arrBorder
    $   APart sh (Range (Z :.  eastY :.  eastX) (Z :.  eastH :.  eastW) inBorder)   arrBorder
    $   APart sh (Range (Z :. northY :. northX) (Z :. northH :. northW) inBorder)   arrBorder
    $   APart sh (Range (Z :. southY :. southX) (Z :. southH :. southW) inBorder)   arrBorder
    $   AUndefined sh


unsafeAppStencilCursor2
        :: Source r a
        => (DIM2 -> Cursor -> Cursor)
        -> Stencil DIM2 a
        -> Array r DIM2 a
        -> Cursor
        -> a

{-# INLINE unsafeAppStencilCursor2 #-}
unsafeAppStencilCursor2 shift
        (StencilStatic sExtent zero loads)
        arr cur0

        | _ :. sHeight :. sWidth        <- sExtent
        , sHeight <= 7, sWidth <= 7
        = let
                -- Get data from the manifest array.
                {-# INLINE getData #-}
                getData (Cursor cur) = arr `unsafeLinearIndex` cur

                -- Build a function to pass data from the array to our stencil.
                {-# INLINE oload #-}
                oload oy ox
                 = let  !cur' = shift (Z :. oy :. ox) cur0
                   in   loads (Z :. oy :. ox) (getData cur')

           in   template7x7 oload zero

        | otherwise
        = error $ unlines 
                [ "mapStencil2: Your stencil is too big for this method."
                , " It must fit within a 7x7 tile to be compiled statically." ]


-- | Like above, but treat elements outside the array has having a constant value.
unsafeAppStencilCursor2_const
        :: forall r a
        .  Source r a
        => (DIM2 -> DIM2 -> DIM2)
        -> Stencil DIM2 a
        -> a
        -> Array r DIM2 a
        -> DIM2
        -> a

{-# INLINE unsafeAppStencilCursor2_const #-}
unsafeAppStencilCursor2_const shift
           (StencilStatic sExtent zero loads)
           fixed arr cur

        | _ :. sHeight      :. sWidth       <- sExtent
        , _ :. (I# aHeight) :. (I# aWidth)  <- extent arr
        , sHeight <= 7, sWidth <= 7
        = let
                -- Get data from the manifest array.
                {-# INLINE getData #-}
                getData :: DIM2 -> a
                getData (Z :. (I# y) :. (I# x))
                 = getData' x y

                {-# NOINLINE getData' #-}
                getData' :: Int# -> Int# -> a
                getData' !x !y
                 | 1# <-   (x <# 0#) `orI#` (x >=# aWidth)
                    `orI#` (y <# 0#) `orI#` (y >=# aHeight)
                 = fixed

                 | otherwise
                 = arr `unsafeIndex` (Z :. (I# y) :.  (I# x))

                -- Build a function to pass data from the array to our stencil.
                {-# INLINE oload #-}
                oload oy ox
                 = let  !cur' = shift (Z :. oy :. ox) cur
                   in   loads (Z :. oy :. ox) (getData cur')

           in   template7x7 oload zero

        | otherwise
        = error $ unlines 
                [ "mapStencil2: Your stencil is too big for this method."
                , " It must fit within a 7x7 tile to be compiled statically." ]


-- | Like above, but clamp out of bounds array values to the closest real value.
unsafeAppStencilCursor2_clamp
        :: forall r a
        .  Source r a
        => (DIM2 -> DIM2 -> DIM2)
        -> Stencil DIM2 a
        -> Array r DIM2 a
        -> DIM2
        -> a

{-# INLINE unsafeAppStencilCursor2_clamp #-}
unsafeAppStencilCursor2_clamp shift
           (StencilStatic sExtent zero loads)
           arr cur

        | _ :. sHeight      :. sWidth       <- sExtent
        , _ :. (I# aHeight) :. (I# aWidth)  <- extent arr
        , sHeight <= 7, sWidth <= 7
        = let
                -- Get data from the manifest array.
                {-# INLINE getData #-}
                getData :: DIM2 -> a
                getData (Z :. (I# y) :. (I# x))
                 = wrapLoadX x y

                {-# NOINLINE wrapLoadX #-}
                wrapLoadX :: Int# -> Int# -> a
                wrapLoadX !x !y
                 | 1# <- x <# 0#        = wrapLoadY 0#             y
                 | 1# <- x >=# aWidth   = wrapLoadY (aWidth -# 1#) y
                 | otherwise    = wrapLoadY x y

                {-# NOINLINE wrapLoadY #-}
                wrapLoadY :: Int# -> Int# -> a
                wrapLoadY !x !y
                 | 1# <- y <#  0#       = loadXY x 0#
                 | 1# <- y >=# aHeight  = loadXY x (aHeight -# 1#)
                 | otherwise     = loadXY x y

                {-# INLINE loadXY #-}
                loadXY :: Int# -> Int# -> a
                loadXY !x !y
                 = arr `unsafeIndex` (Z :. (I# y) :.  (I# x))

                -- Build a function to pass data from the array to our stencil.
                {-# INLINE oload #-}
                oload oy ox
                 = let  !cur' = shift (Z :. oy :. ox) cur
                   in   loads (Z :. oy :. ox) (getData cur')

           in   template7x7 oload zero

        | otherwise
        = error $ unlines 
                [ "mapStencil2: Your stencil is too big for this method."
                , " It must fit within a 7x7 tile to be compiled statically." ]


-- | Data template for stencils up to 7x7.
template7x7
        :: (Int -> Int -> a -> a)
        -> a -> a

{-# INLINE template7x7 #-}
template7x7 f zero
        = f (-3) (-3)  $  f (-3) (-2)  $  f (-3) (-1)  $  f (-3)   0  $  f (-3)   1  $  f (-3)   2  $ f (-3) 3
        $ f (-2) (-3)  $  f (-2) (-2)  $  f (-2) (-1)  $  f (-2)   0  $  f (-2)   1  $  f (-2)   2  $ f (-2) 3
        $ f (-1) (-3)  $  f (-1) (-2)  $  f (-1) (-1)  $  f (-1)   0  $  f (-1)   1  $  f (-1)   2  $ f (-1) 3
        $ f   0  (-3)  $  f   0  (-2)  $  f   0  (-1)  $  f   0    0  $  f   0    1  $  f   0    2  $ f   0  3
        $ f   1  (-3)  $  f   1  (-2)  $  f   1  (-1)  $  f   1    0  $  f   1    1  $  f   1    2  $ f   1  3
        $ f   2  (-3)  $  f   2  (-2)  $  f   2  (-1)  $  f   2    0  $  f   2    1  $  f   2    2  $ f   2  3
        $ f   3  (-3)  $  f   3  (-2)  $  f   3  (-1)  $  f   3    0  $  f   3    1  $  f   3    2  $ f   3  3
        $ zero

