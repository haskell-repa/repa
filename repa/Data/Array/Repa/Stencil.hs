{-# LANGUAGE PackageImports, MagicHash #-}
{-# OPTIONS -Wnot #-}

-- | Class of types that can be used as array shapes and indices.
module Data.Array.Repa.Stencil
	( Stencil(..)
	, relaxLaplace)
where
import Data.Array.Repa			as R
import qualified Data.Array.Repa.Shape	as S
import GHC.Exts

data Stencil sh a b
	= Stencil
	{ stencilExtent	:: !sh
	, stencilZero	:: !b 
	, stencilAcc	:: !(sh -> a -> b -> b) }
	
	
-- Make a generic version that works on stencils of arbitrary dimension.
-- | Also provide versions specific to array dimensions.

relaxLaplace :: Array DIM2 Double -> Array DIM2 Double
relaxLaplace arr@Manifest{} 
	= force $ R.map (/ 4) (mapStencil2 laplace  arr)


-- Apply a stencil to every element of an array
--   This is specialised for 3x3 and 5x5 stencils. 
--   Smaller kernels that fit into one of the specialised sizes are still specialised (like 3x2)
mapStencil2 
	:: (Elt a, Elt b)
	=> Stencil DIM2 a b -> Array DIM2 a -> Array DIM2 b

{-# INLINE mapStencil2 #-}
mapStencil2 stencil arr
	= fromFunction (extent arr) (unsafeAppStencil2 stencil arr)


-- | Apply a stencil to a single position in an image.
--	Applying it too close to the border yields badness.
unsafeAppStencil2
	:: (Elt a, Elt b)
	=> Stencil DIM2 a b -> Array DIM2 a -> DIM2 -> b

{-# INLINE unsafeAppStencil2 #-}
unsafeAppStencil2 stencil@(Stencil sExtent zero load) arr ix@(Z :. y :. x)
 = case sExtent of
    Z :. 3 :. 3
       -> oload (-1) (-1)  $  oload (-1)   0  $  oload (-1)   1
	$ oload   0  (-1)  $  oload   0    0  $  oload   0    1
	$ oload   1  (-1)  $  oload   1    0  $  oload   1    1
	$ zero

    Z :. 5 :. 5
       -> oload (-2) (-2)  $ oload (-2) (-1)  $  oload (-2)   0  $  oload (-2)   1  $  oload (-2)   2 
	$ oload (-1) (-2)  $ oload (-1) (-1)  $  oload (-1)   0  $  oload (-1)   1  $  oload (-1)   2 
	$ oload   0  (-2)  $ oload   0  (-1)  $  oload   0    0  $  oload   0    1  $  oload   0    2  
	$ oload   1  (-2)  $ oload   1  (-1)  $  oload   1    0  $  oload   1    1  $  oload   1    2 
	$ oload   2  (-2)  $ oload   2  (-1)  $  oload   2    0  $  oload   2    1  $  oload   2    2 
	$ zero

 where	{-# INLINE oload #-}
	oload oy ox	
	 = load (Z :. oy :. ox) (arr !: (Z :. y + oy :. x + ox))



makeConvolution
	:: (Elt a, Num a) 
	=> sh -> (sh -> Maybe a) -> Stencil sh a a

{-# INLINE makeConvolution #-}
makeConvolution ex getCoeff
 = Stencil ex 0 
 $ \ix val acc
	-> case getCoeff ix of
		Nothing		-> acc
		Just coeff	-> acc + val * coeff
			

-- | We're using a Nothing here to represent an unused element instead of zero,
--   because there is no core rule  x# *## 0#. This is omitted from the base
--   libraries because it breaks NaN handling.
-- 
laplace :: (Elt a, Num a) => Stencil DIM2 a a
{-# INLINE laplace #-}
laplace 
 = makeConvolution (Z :. 3 :. 3)
 $ \ix -> case ix of
		Z :.  0  :.  1	-> Just 1
		Z :.  0  :. -1	-> Just 1
		Z :.  1  :.  0	-> Just 1
		Z :. -1  :.  0	-> Just 1
		_		-> Nothing
