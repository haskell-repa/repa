{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, FlexibleContexts #-}
module Main where
import Data.Array.Repa
import Data.Array.Repa.Arbitrary(forAll2UShaped, forAll2VShaped)
import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad            (liftM2)

-- | Example of how write unit tests for Repa arrays of a given number of dimensions
arrayAdditionU sh 
 = forAll2UShaped sh test
 where  test (a, b)   = a +^ b == fromFunction (extent a) (\i -> ((a!i) + (b!i) :: Double))

arrayAdditionV sh 
 = forAll2VShaped sh test
 where  test (a, b)   = a +^ b == fromFunction (extent a) (\i -> ((a!i) + (b!i) :: Double))

prop_addU1D :: DIM1 -> Property
prop_addU1D = arrayAdditionU
 
prop_addU2D :: DIM2 -> Property
prop_addU2D = arrayAdditionU

prop_addU3D :: DIM3 -> Property
prop_addU3D = arrayAdditionU
 
prop_addU4D :: DIM4 -> Property
prop_addU4D = arrayAdditionU
 
prop_addU5D :: DIM5 -> Property
prop_addU5D = arrayAdditionU
 
prop_addV1D :: DIM1 -> Property
prop_addV1D = arrayAdditionV
 
prop_addV2D :: DIM2 -> Property
prop_addV2D = arrayAdditionV

prop_addV3D :: DIM3 -> Property
prop_addV3D = arrayAdditionV
 
prop_addV4D :: DIM4 -> Property
prop_addV4D = arrayAdditionV
 
prop_addV5D :: DIM5 -> Property
prop_addV5D = arrayAdditionV
 
main = $quickCheckAll
