{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ArrayExamples (
  transpose,
  transposePrim,
  transposeDFT,
  relax,
  relaxShift,
  mmMult)
         
where

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Unlifted ((:*:)(..))

import Array 

--  module ArrayExamples ...
--  =========================
--
--  some examples using strict arrays
--

-- Matrix transposition
------------------------

-- uses the 'standard' library functions
transpose:: U.Elt e => Array DIM2 e -> Array DIM2 e
{-# INLINE transpose #-}
transpose arr = 
  backpermute arr (() :.n :. m) (\((() :. i) :. j) -> ((() :. j) :. i))
  where
    (() :.n :. m) = arrayShape arr


-- avoids index/tuple calculations
transposePrim:: U.Elt e => Array DIM2 e -> Array DIM2 e
{-# INLINE transposePrim #-}
transposePrim arr = arr{arrayData = U.bpermute (arrayData arr) inds}
  where
   (_ :. n) = arrayShape arr
   inds      = U.zipWith (+)
                 (U.repeat n n (U.enumFromStepLen 0 n n))
                 (U.replicate_s (U.lengthsToSegd  $ U.replicate n n) (U.enumFromTo 0 (n-1)))


-- uses default backpermute 
transposeDFT:: (U.Elt a, Num a) => Array DIM2 a ->        Array DIM2 a
{-# INLINE transposeDFT #-}
transposeDFT arr = assert (n==m) $
  backpermuteDft arr 0 (arrayShape arr) (\((() :. i) :. j) -> Just ((() :. j) :. i))
  where
    (() :.n :. m) = arrayShape arr



-- Matrix relaxation
-- -----------------

relax:: (U.Elt a, Fractional a) => Array DIM2 a -> Array DIM2 a
{-# INLINE relax #-}
relax arr =  
  Array.map (/ 5) $ 
    Array.zipWith (+) 
    (Array.zipWith (+) (Array.zipWith (+) shiftu arr) shiftl) (Array.zipWith (+) shiftr shiftd)
  where
    s@((() :. n) :. m) = arrayShape arr
    shiftu = backpermuteDft arr 0 s  fu 
    shiftd = backpermuteDft arr 0 s  fd 
    shiftl = backpermuteDft arr 0 s  fl 
    shiftr = backpermuteDft arr 0 s  fr 
    fu = \((() :. i) :. j) -> if (i < (n-1)) then Just (() :. (i+1) :. j) else Nothing
    fd = \((() :. i) :. j) -> if (i > 0)     then Just (() :. (i-1) :. j) else Nothing
    fl = \((() :. i) :. j) -> if (j < (m-1)) then Just (() :. i :. (j+1)) else Nothing
    fr = \((() :. i) :. j) -> if (j > 0)     then Just (() :. i :. (j-1)) else Nothing

relaxShift:: Array DIM2 Double -> Array DIM2 Double
{-# INLINE relaxShift #-}
relaxShift arr =  
  Array.map (/ 5) $ 
    Array.zipWith (+) 
    (Array.zipWith (+) (Array.zipWith (+) shiftu arr) shiftl) (Array.zipWith (+) shiftr shiftd)
  where
    s@((() :. n) :. m) = arrayShape arr
    shiftu = shift arr 0 ((():. 1   :. 0)::DIM2)
    shiftd = shift arr 0 ((():. (-1) :. 0)::DIM2)
    shiftl = shift arr 0 ((():. 0   :. 1)::DIM2)
    shiftr = shift arr 0 ((():. 0   :. (-1))::DIM2)



-- Matrix Multiplication
-- ---------------------


mmMult:: Array DIM2 Double -> Array DIM2 Double -> Array DIM2 Double
mmMult arr1 arr2 = assert (m1 == n2) $ 
  mapFold (+) 0 $ Array.zipWith (*) arr1Ext arr2Ext
  where
    arr1Ext = Array.replicate arr1 (IndexAll (IndexFixed m2 (IndexAll IndexNil)))
    arr2Ext = Array.replicate (transpose arr2) (IndexAll (IndexAll (IndexFixed n1 IndexNil)))
    (() :. m1 :. n1) = arrayShape arr1
    (() :. m2 :. n2) = arrayShape arr2



