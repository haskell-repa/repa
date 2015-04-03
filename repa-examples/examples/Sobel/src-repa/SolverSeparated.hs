{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module SolverSeparated
        ( Image
        , gradientX_sep
        , gradientY_sep )
where
import Data.Array.Repa                  as Repa
import Data.Array.Repa.Stencil

type Image      = Array DIM2 Float

gradientX_sep :: Image -> Image
gradientX_sep = gradientX1 . gradientX2

gradientX1 :: Image -> Image
{-# NOINLINE gradientX1 #-}
gradientX1 img
        = force2
        $ forStencil2 BoundClamp img
          [stencil2|    1 0 -1 |]

gradientX2 :: Image -> Image
{-# NOINLINE gradientX2 #-}
gradientX2 img
        = force2
        $ forStencil2 BoundClamp img
          [stencil2|    1
                        2
                        1 |]


gradientY_sep :: Image -> Image
gradientY_sep = gradientY1 . gradientY2

gradientY1 :: Image -> Image
{-# NOINLINE gradientY1 #-}
gradientY1 img
        = force2
        $ forStencil2 BoundClamp img
          [stencil2|    1 2 1 |]

gradientY2 :: Image -> Image
{-# NOINLINE gradientY2 #-}
gradientY2 img
        = force2
        $ forStencil2 BoundClamp img
          [stencil2|     1
                         0
                        -1 |]
