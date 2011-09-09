

-- | TODO: Use local rules to rewrite these to use special versions
--         for specific representations, eg for partitioned arrays 
--         we want to map the regions separately.
module Data.Array.Repa.Operators.Mapping
        ( map
        , zipWith
        , (+^), (-^), (*^), (/^))
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Repr.Delayed
import Prelude hiding (map, zipWith)


map     :: (Shape sh, Repr r a)
        => (a -> b) -> Array r sh a -> Array D sh b
{-# INLINE map #-}
map f arr
 = case load arr of
        Delayed sh g    -> Delayed sh (f . g)



zipWith :: (Shape sh, Repr r1 a, Repr r2 b)
        => (a -> b -> c)
        -> Array r1 sh a -> Array r2 sh b
        -> Array D sh c
{-# INLINE zipWith #-}
zipWith f arr1 arr2
 = let  {-# INLINE getElem' #-}
	getElem' ix	= f (arr1 `unsafeIndex` ix) (arr2 `unsafeIndex` ix)
   in	fromFunction
		(intersectDim (extent arr1) (extent arr2))
		getElem'


{-# INLINE (+^) #-}
(+^)	= zipWith (+)


{-# INLINE (-^) #-}
(-^)	= zipWith (-)

{-# INLINE (*^) #-}
(*^)	= zipWith (*)

{-# INLINE (/^) #-}
(/^)	= zipWith (/)
