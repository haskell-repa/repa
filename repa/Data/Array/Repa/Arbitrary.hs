{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Array.Repa.Arbitrary
        ( -- * Arbitrary Unboxed Arrays
          arbitraryUShaped
        , forAllUShaped
        , forAll2UShaped
        , forAll3UShaped
        , forAll4UShaped
        , forAll5UShaped

          -- * Arbitrary Boxed Arrays
        , arbitraryVShaped
        , forAllVShaped
        , forAll2VShaped
        , forAll3VShaped
        , forAll4VShaped
        , forAll5VShaped)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa.Shape
import Data.Array.Repa.Index
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Property                 (forAll)
import Control.Monad
import qualified Data.Array.Repa.Repr.Vector    as V
import qualified Data.Vector.Unboxed            as U


-- Aribrary -------------------------------------------------------------------
-- | This module exports instances of @Arbitrary@ and @CoArbitrary@ for
--   unboxed Repa arrays.
instance Arbitrary Z where
  arbitrary = return Z


-- Note: this is a shape that is "sized", and then random array for a given
-- shape is generated.
instance Arbitrary a 
      => Arbitrary (a :. Int) where
 arbitrary 
        = sized (\n -> do 
                b <- if n == 0
                         then return 1
                         else choose (1, n)
                a <- resize ((n + b - 1) `div` b) arbitrary
                -- each dimension should be at least 1-wide
                return $ a :. b)


-- | Generates a random unboxed array of a given shape
arbitraryUShaped sh =   fromListUnboxed sh `fmap` vector (size sh)


-- | Generates a random boxed array of a given shape
arbitraryVShaped sh = V.fromListVector  sh `fmap` vector (size sh)


instance (Arbitrary sh, Arbitrary a, U.Unbox a, Shape sh) 
       => Arbitrary (Array U sh a) where
  arbitrary = arbitrary >>= arbitraryUShaped


instance (Arbitrary sh, Arbitrary a, Shape sh) 
       => Arbitrary (Array V.V sh a) where
  arbitrary = arbitrary >>= arbitraryVShaped


-- CoArbitrary ----------------------------------------------------------------
instance CoArbitrary Z where
  coarbitrary _ = id 

instance (CoArbitrary a) 
       => CoArbitrary (a :. Int) where
  coarbitrary (a :. b) = coarbitrary a . coarbitrary b

instance (CoArbitrary sh, CoArbitrary a, Source r a, Shape sh) 
       => CoArbitrary (Array r sh a) where
  coarbitrary arr 
        = (coarbitrary . extent $ arr) . (coarbitrary . toList $ arr)


-- Wrappers -------------------------------------------------------------------
-- | Convenience functions for writing tests on 2-,3-,4-tuples of arrays
--   of the same size (or just of a fixed size.)

-- | These are helper functions:
forAll2 arbf = forAll $ liftM2 (,)    arbf arbf
forAll3 arbf = forAll $ liftM3 (,,)   arbf arbf arbf
forAll4 arbf = forAll $ liftM4 (,,,)  arbf arbf arbf arbf
forAll5 arbf = forAll $ liftM5 (,,,,) arbf arbf arbf arbf arbf


-- | Property tested for unboxed random arrays with a given shape.
forAllUShaped sh  = forAll  $ arbitraryUShaped sh

-- | Property tested for pair of unboxed random arrays with a given shape.
forAll2UShaped sh = forAll2 $ arbitraryUShaped sh

-- | Property tested for triple of unboxed random arrays with a given shape.
forAll3UShaped sh = forAll3 $ arbitraryUShaped sh

-- | Property tested for quadruple of unboxed random arrays with a given shape.
forAll4UShaped sh = forAll4 $ arbitraryUShaped sh

-- | Property tested for 5-tuple of unboxed random arrays with a given shape.
forAll5UShaped sh = forAll5 $ arbitraryUShaped sh


-- | Property tested for unboxed random arrays with a given shape.
forAllVShaped sh  = forAll  $ arbitraryVShaped sh

-- | Property tested for pair of unboxed random arrays with a given shape.
forAll2VShaped sh = forAll2 $ arbitraryVShaped sh

-- | Property tested for triple of unboxed random arrays with a given shape.
forAll3VShaped sh = forAll3 $ arbitraryVShaped sh

-- | Property tested for quadruple of unboxed random arrays with a given shape.
forAll4VShaped sh = forAll4 $ arbitraryVShaped sh

-- | Property tested for 5-tuple of unboxed random arrays with a given shape.
forAll5VShaped sh = forAll5 $ arbitraryVShaped sh
