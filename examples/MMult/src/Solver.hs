{-# LANGUAGE TypeOperators #-}

module Solver where
import Data.Array.Repa

mmMult	:: Array DIM2 Double
	-> Array DIM2 Double
	-> Array DIM2 Double

mmMult arr1@Manifest{} arr2@Manifest{}
	= fold (+) 0 
  	$ traverse2 arr1 (force $ transpose arr2)
      		(\(sh :. m1 :. n1) -> \(_ :. n2 :. m2) -> (sh :. m1 :. n2 :. n1))
		(\f1 -> \f2 -> \(sh :. i :. j :. k) -> f1 (sh :. i :. k) * f2 (sh :. j :. k))
