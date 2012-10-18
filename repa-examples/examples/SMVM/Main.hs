{-# LANGUAGE BangPatterns, MagicHash #-}

import Solver
import Data.Array.Repa                  as R
import Data.Array.Repa.Vector.Segd



main
 = do   let !matrix     = fromListUnboxed (Z :. 7)
                        $ [ (0, 1.0), (1, 0.2)
                          , (0, 0.2), (1, 1.0), (2, 0.4)
                                    , (1, 0.4), (2, 1.0) ]

        let !segd       = Segd
                        ( fromListUnboxed (Z :. 3)
                          [ 2, 3, 2 ])
                        ( fromListUnboxed (Z :. 3)
                          [ 0, 2, 5 ])
                          7#

        let !vector     = fromListUnboxed (Z :. 3)
                        $ [ 1.0, 1.0, 1.0 ]

        vec'    <- smvm segd matrix vector

        print $ vec'

