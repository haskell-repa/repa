{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Hierarchical where 


import qualified Array as A
import qualified DArray as DA
import qualified DArrayExamples as DAE


import qualified Data.Array.Parallel.Unlifted as U

import Control.Exception (evaluate, assert)
import System.Console.GetOpt
import qualified System.Random as R

import  Data.Array.Parallel.Unlifted  ((:*:)(..))
import qualified Data.Array.Parallel.Unlifted as U
import Control.Exception (evaluate)


data HMatrix a =  HMatrix { hmThreshold:: Int
                          , hmOrder    :: Int
                          , hmDA       ::  DA.DArray A.DIM2 a
                          }


-- Just changes the representation, not the ordering of the elements.
--
toHMatrix:: (U.Elt a) => Int -> A.Array A.DIM2 a -> HMatrix a
{-# INLINE toHMatrix #-}
toHMatrix threshold arr =
   HMatrix{ hmThreshold =  threshold
          , hmOrder     =  order
          , hmDA        =  DA.toDArray  arr
          }
   where
     (_:*: order) = A.arrayShape arr


 

($$):: (DA.DArray A.DIM2 a -> b) -> HMatrix a -> b
($$) f hm = f $ hmDA hm

hmSplit:: U.Elt a => HMatrix a -> (HMatrix a, HMatrix a, HMatrix a, HMatrix a)
{-# INLINE hmSplit #-}
hmSplit hm = assert (hmOrder hm > 1) $
  (t1,t2,t3,t4)
  where
    n2 = (hmOrder hm) `div` 2
    t1 = hm{ hmOrder = n2, hmDA    = (DA.tile $$ hm) (():*: 0 :*: 0)   (() :*: n2 :*: n2)}
    t2 = hm{ hmOrder = n2, hmDA    = (DA.tile $$ hm) (():*: n2 :*: 0)  (() :*: n2 :*: n2)}
    t3 = hm{ hmOrder = n2, hmDA    = (DA.tile $$ hm) (():*: 0 :*: n2)  (() :*: n2 :*: n2)}
    t4 = hm{ hmOrder = n2, hmDA    = (DA.tile $$ hm) (():*: n2 :*: n2) (() :*: n2 :*: n2)}

hmJoin:: U.Elt a => (HMatrix a, HMatrix a, HMatrix a, HMatrix a) -> HMatrix a 
{-# INLINE hmJoin #-}
hmJoin (t1,t2,t3,t4) = assert (hmOrder t1 > 1) $
    t1{hmDA = DA.append (DA.append (hmDA t1) (hmDA t2) (() :*: 2 * order :*: order)) 
                        (DA.append (hmDA t3) (hmDA t4) (() :*: 2 * order :*: order))
                        (() :*: 2* order :*: 2* order)}
  where
    order = hmOrder t1    



--  matrix multiplication, assumes square matrices of size 2^n * 2^n
--
hmmult:: HMatrix Double -> HMatrix Double -> HMatrix Double
hmmult a b = 
    if (n <= hmThreshold a) 
    then a{hmDA = DAE.mmMult (DA.forceDArray $ hmDA a) (DA.forceDArray $ hmDA b)}
    else hmJoin (r1,r2,r3,r4)

  where
    n = hmOrder a
    m = hmOrder b

    (a1,a2,a3,a4) = hmSplit a
    (b1,b2,b3,b4) = hmSplit b
  
    r1 = a1{hmDA = (DA.zipWith (+) $$ (hmmult a1 b1)) $$ (hmmult a2 b3)}
    r2 = a2{hmDA = (DA.zipWith (+) $$ (hmmult a1 b2)) $$ (hmmult a2 b4)}
    r3 = a3{hmDA = (DA.zipWith (+) $$ (hmmult a3 b1)) $$ (hmmult a4 b3)}
    r4 = a4{hmDA = (DA.zipWith (+) $$ (hmmult a3 b2)) $$ (hmmult a4 b4)}

--
--  Hierarchical (one dim)
--  ======================


data HHMatrix a =  HHMatrix { hhmThreshold:: Int
                            , hhmOrder    :: Int
                            , hhmDA       :: DA.DArray A.DIM1 a
                            }


-- Just changes the representation, not the ordering of the elements.
--
toHHMatrix:: (U.Elt a) => Int -> Int -> A.Array A.DIM1 a -> HHMatrix a
{-# INLINE toHHMatrix #-}
toHHMatrix order threshold arr =
   HHMatrix{ hhmThreshold =  threshold
          , hhmOrder     =  order
          , hhmDA        =  DA.toDArray  arr
          }



 

($$$):: (DA.DArray A.DIM1 a -> b) -> HHMatrix a -> b
($$$) f hm = f $ hhmDA hm

hhmSplit:: U.Elt a => HHMatrix a -> (HHMatrix a, HHMatrix a, HHMatrix a, HHMatrix a)
{-# INLINE hhmSplit #-}
hhmSplit hm = assert (hhmOrder hm > 1) $
  (t1,t2,t3,t4)
  where
    n2 = (hhmOrder hm) `div` 2
    t1 = hm{ hhmOrder = n2, hhmDA    = (DA.tile $$$ hm) (():*: 0)       (() :*: n2*n2)}
    t2 = hm{ hhmOrder = n2, hhmDA    = (DA.tile $$$ hm) (():*: n2*n2)   (() :*: n2*n2)}
    t3 = hm{ hhmOrder = n2, hhmDA    = (DA.tile $$$ hm) (():*: 2*n2*n2) (() :*: n2*n2)}
    t4 = hm{ hhmOrder = n2, hhmDA    = (DA.tile $$$ hm) (():*: 3*n2*n2) (() :*: n2*n2)}

hhmJoin:: U.Elt a => (HHMatrix a, HHMatrix a, HHMatrix a, HHMatrix a) -> HHMatrix a 
{-# INLINE hhmJoin #-}
hhmJoin (t1,t2,t3,t4) = assert (hhmOrder t1 > 1) $
    t1{hhmDA = DA.append 
                (DA.append (hhmDA t1) (hhmDA t2) (() :*: 2*order*order)) 
                (DA.append (hhmDA t3) (hhmDA t4) (() :*: 2*order*order))
                        (() :*: 4*order*order)}
  where
    order = hhmOrder t1    

--  matrix multiplication, assumes square matrices of size 2^n * 2^n
--
hhmmult:: HHMatrix Double -> HHMatrix Double -> HHMatrix Double
hhmmult a b = 
    if (n <= hhmThreshold a) 
    then a{hhmDA = DA.reshape
                    (DAE.mmMult (DA.toDArray$ A.reshape (DA.fromDArray$ hhmDA a) (():*: n:*:n)) 
                                     (DA.toDArray$ A.reshape (DA.fromDArray$ hhmDA b) (():*: n:*:n)))
                    (() :*: n*n) 
          }
    else hhmJoin (r1,r2,r3,r4)

  where
    n = hhmOrder a
    m = hhmOrder b

    (a1,a2,a3,a4) = hhmSplit a
    (b1,b2,b3,b4) = hhmSplit b
  
    r1 = a1{hhmDA = (DA.zipWith (+) $$$ (hhmmult a1 b1)) $$$ (hhmmult a2 b3)}
    r2 = a2{hhmDA = (DA.zipWith (+) $$$ (hhmmult a1 b2)) $$$ (hhmmult a2 b4)}
    r3 = a3{hhmDA = (DA.zipWith (+) $$$ (hhmmult a3 b1)) $$$ (hhmmult a4 b3)}
    r4 = a4{hhmDA = (DA.zipWith (+) $$$ (hhmmult a3 b2)) $$$ (hhmmult a4 b4)}
