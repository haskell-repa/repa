{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

module Data.Array.Repa.Operators.Mapping
        ( -- * Generic maps
          map
        , zipWith
        , (+^), (-^), (*^), (/^)

          -- * Combining maps
        , Combine(..))
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Repr.ByteString
import Data.Array.Repa.Repr.Cursored
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.Hint
import Data.Array.Repa.Repr.Partitioned
import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa.Repr.Undefined
import Prelude hiding (map, zipWith)
import Foreign.Storable
import Data.Word

-- | Apply a worker function to each element of an array, 
--   yielding a new array with the same extent.
--
map     :: (Shape sh, Repr r a)
        => (a -> b) -> Array r sh a -> Array D sh b
{-# INLINE [3] map #-}
map f arr
 = case delay arr of
        ADelayed sh g -> ADelayed sh (f . g)


-- ZipWith --------------------------------------------------------------------
-- | Combine two arrays, element-wise, with a binary operator.
--	If the extent of the two array arguments differ,
--	then the resulting array's extent is their intersection.
--
zipWith :: (Shape sh, Repr r1 a, Repr r2 b)
        => (a -> b -> c)
        -> Array r1 sh a -> Array r2 sh b
        -> Array D sh c
{-# INLINE [2] zipWith #-}
zipWith f arr1 arr2
 = arr1 `deepSeqArray` arr2 `deepSeqArray`
   let 
        {-# INLINE get #-}
        get ix  = f (arr1 `unsafeIndex` ix) (arr2 `unsafeIndex` ix)

   in   fromFunction 
                (intersectDim (extent arr1) (extent arr2)) 
                get


{-# INLINE (+^) #-}
(+^)	= zipWith (+)

{-# INLINE (-^) #-}
(-^)	= zipWith (-)

{-# INLINE (*^) #-}
(*^)	= zipWith (*)

{-# INLINE (/^) #-}
(/^)	= zipWith (/)



-- Combine --------------------------------------------------------------------
-- | Combining versions of @map@ and @zipWith@ that preserve the representation
--   of cursored and partitioned arrays. 
--
--   For cursored (@C@) arrays, the cursoring of the source array is preserved.
--
--   For partitioned (@P@) arrays, the worker function is fused with each array
--   partition separately, instead of treating the whole array as a single
--   bulk object. 
--
--   Preserving the cursored and\/or paritioned representation of an array 
--   is will make follow-on computation more efficient than if the array was
--   converted to a vanilla Delayed (@D@) array as with plain `map` and `zipWith`.
--
--   If the source array is not cursored or partitioned then `cmap` and 
--   `czipWith` are identical to the plain functions.
--
class Combine r1 a r2 b | r1 -> r2 where

 -- | Combining @map@.
 cmap   :: Shape sh 
        => (a -> b) 
        -> Array r1 sh a 
        -> Array r2 sh b

 -- | Combining @zipWith@.
 --   If you have a cursored or partitioned source array then use that as
 --   the third argument (corresponding to @r1@ here)
 czipWith
        :: (Shape sh, Repr r c)
        => (c -> a -> b)
        -> Array r  sh c
        -> Array r1 sh a
        -> Array r2 sh b


-- ByteString -------------------------
instance Combine B Word8 D b where
 cmap           = map
 czipWith       = zipWith


-- Cursored ---------------------------
instance Combine C a C b where
 {-# INLINE [3] cmap #-}
 cmap f (ACursored sh makec shiftc loadc)
        = ACursored sh makec shiftc (f . loadc)

 {-# INLINE [2] czipWith #-}
 czipWith f arr1 (ACursored sh makec shiftc loadc)
  = let {-# INLINE makec' #-}
        makec' ix               = (ix, makec ix)

        {-# INLINE shiftc' #-}
        shiftc' off (ix, cur)   = (addDim off ix, shiftc off cur)

        {-# INLINE load' #-}
        load' (ix, cur)         = f (arr1 `index` ix) (loadc cur)

    in  ACursored 
                (intersectDim (extent arr1) sh)
                makec' shiftc' load'


-- Delayed ----------------------------
instance Combine D a D b where
 cmap           = map
 czipWith       = zipWith


-- ForeignPtr -------------------------
instance Storable a => Combine F a D b where
 cmap           = map
 czipWith       = zipWith


-- Partitioned ------------------------
instance (Combine r11 a r21 b
        , Combine r12 a r22 b)
       => Combine (P r11 r12) a (P r21 r22) b where

 cmap f (APart sh range arr1 arr2)
        = APart sh range (cmap f arr1) (cmap f arr2)
 {-# INLINE [3] cmap #-}

 czipWith f arr1 (APart sh range arr21 arr22)
        = APart sh range (czipWith f arr1 arr21)
                         (czipWith f arr1 arr22)
 {-# INLINE [2] czipWith #-}


-- Small ------------------------------
instance   Combine r1 a r2 b
        => Combine (S r1) a (S r2) b where
 cmap f (ASmall arr1)
        = ASmall (cmap f arr1)
 {-# INLINE [3] cmap #-}

 czipWith f arr1 (ASmall arr2)
        = ASmall (czipWith f arr1 arr2)
 {-# INLINE [3] czipWith #-}


-- Unboxed ----------------------------
instance Unbox a => Combine U a D b where
 cmap           = map
 czipWith       = zipWith


-- Undefined --------------------------
instance Combine X a X b where
 cmap     _   (AUndefined sh) = AUndefined sh
 czipWith _ _ (AUndefined sh) = AUndefined sh


 
