{-# LANGUAGE BangPatterns, MagicHash #-}

import Solver
import Data.Array.Repa.Vector                   as R
import qualified Data.Array.Repa.Vector.Segd    as Segd

main
 = do   let !matrix     = fromListUnboxed (Z :. 7)
                        $ [ (0, 1.0), (1, 0.2)
                          , (0, 0.2), (1, 1.0), (2, 0.4)
                                    , (1, 0.4), (2, 1.0) ]

        let !segd       = Segd.fromLengths 
                                (fromListUnboxed (Z :. 3) [ 2, 3, 2 ])

        let !vector     = fromListUnboxed (Z :. 3)
                                [ 1.0, 1.0, 1.0 ]

        let !vec'       = smvm segd matrix vector

        print $ vec'

