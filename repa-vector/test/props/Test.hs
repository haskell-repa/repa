{-# LANGUAGE TypeOperators, FlexibleContexts #-}

import Test.QuickCheck
import Data.Array.Repa


instance Num Z where
 Z + Z         = Z
 Z - Z         = Z
 Z * Z         = Z
 abs Z         = Z
 signum Z      = 0

 fromInteger 0 = Z
 fromInteger _ = error "fromInteger[Z]: non-zero integer"


instance Arbitrary Z where
 arbitrary = return Z
 

instance (Arbitrary a, Arbitrary (Positive b))
       => Arbitrary (a :. b) where
 arbitrary
  = do  x1              <- arbitrary
        Positive x2     <- arbitrary
        return (x1 :. x2)



