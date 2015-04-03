{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

module Data.Array.Repa.Operators.Mapping
        ( -- * Generic maps
          map
        , zipWith
        , (+^), (-^), (*^), (/^)

          -- * Structured maps
        , Structured(..))
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base
import Data.Array.Repa.Repr.ByteString
import Data.Array.Repa.Repr.Cursored
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.HintSmall
import Data.Array.Repa.Repr.HintInterleave
import Data.Array.Repa.Repr.Partitioned
import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa.Repr.Undefined
import Prelude hiding (map, zipWith)
import Foreign.Storable
import Data.Word

-- | Apply a worker function to each element of an array, 
--   yielding a new array with the same extent.
--
map     :: (Shape sh, Source r a)
        => (a -> b) -> Array r sh a -> Array D sh b
map f arr
 = case delay arr of
        ADelayed sh g -> ADelayed sh (f . g)
{-# INLINE [3] map #-}


-- ZipWith --------------------------------------------------------------------
-- | Combine two arrays, element-wise, with a binary operator.
--      If the extent of the two array arguments differ,
--      then the resulting array's extent is their intersection.
--
zipWith :: (Shape sh, Source r1 a, Source r2 b)
        => (a -> b -> c)
        -> Array r1 sh a -> Array r2 sh b
        -> Array D sh c
zipWith f arr1 arr2
 = let  get ix  = f (arr1 `unsafeIndex` ix) (arr2 `unsafeIndex` ix)
        {-# INLINE get #-}
        
   in   fromFunction 
                (intersectDim (extent arr1) (extent arr2)) 
                get
{-# INLINE [2] zipWith #-}

infixl 7  *^, /^
infixl 6  +^, -^

(+^)    = zipWith (+)
{-# INLINE (+^) #-}

(-^)    = zipWith (-)
{-# INLINE (-^) #-}

(*^)    = zipWith (*)
{-# INLINE (*^) #-}

(/^)    = zipWith (/)
{-# INLINE (/^) #-}


-- Structured -------------------------------------------------------------------
-- | Structured versions of @map@ and @zipWith@ that preserve the representation
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
--   If the source array is not cursored or partitioned then `smap` and 
--   `szipWith` are identical to the plain functions.
--
class Structured r1 a b where
 -- | The target result representation.
 type TR r1

 -- | Structured @map@.
 smap   :: Shape sh 
        => (a -> b) 
        -> Array r1     sh a 
        -> Array (TR r1) sh b

 -- | Structured @zipWith@.
 --   If you have a cursored or partitioned source array then use that as
 --   the third argument (corresponding to @r1@ here)
 szipWith
        :: (Shape sh, Source r c)
        => (c -> a -> b)
        -> Array r      sh c
        -> Array r1     sh a
        -> Array (TR r1) sh b


-- ByteString -------------------------
instance Structured B Word8 b where
 type TR B = D
 smap           = map
 szipWith       = zipWith


-- Cursored ---------------------------
instance Structured C a b where
 type TR C = C

 smap f (ACursored sh makec shiftc loadc)
        = ACursored sh makec shiftc (f . loadc)
 {-# INLINE [3] smap #-}

 szipWith f arr1 (ACursored sh makec shiftc loadc)
  = let makec' ix               = (ix, makec ix)
        {-# INLINE makec' #-}
        
        shiftc' off (ix, cur)   = (addDim off ix, shiftc off cur)
        {-# INLINE shiftc' #-}

        load' (ix, cur)         = f (arr1 `unsafeIndex` ix) (loadc cur)
        {-# INLINE load' #-}

    in  ACursored 
                (intersectDim (extent arr1) sh)
                makec' shiftc' load'
 {-# INLINE [2] szipWith #-}


-- Delayed ----------------------------
instance Structured D a b where
 type TR D = D
 smap           = map
 szipWith       = zipWith


-- ForeignPtr -------------------------
instance Storable a => Structured F a b where
 type TR F = D
 smap           = map
 szipWith       = zipWith


-- Partitioned ------------------------
instance (Structured r1 a b
        , Structured r2 a b)
       => Structured (P r1 r2) a b where
 type TR (P r1 r2) = P (TR r1) (TR r2)

 smap f (APart sh range arr1 arr2)
        = APart sh range (smap f arr1) (smap f arr2)
 {-# INLINE [3] smap #-}

 szipWith f arr1 (APart sh range arr21 arr22)
        = APart sh range (szipWith f arr1 arr21)
                         (szipWith f arr1 arr22)
 {-# INLINE [2] szipWith #-}


-- Small ------------------------------
instance   Structured r1 a b
        => Structured (S r1) a b where
 type TR (S r1) = S (TR r1)

 smap f (ASmall arr1)
        = ASmall (smap f arr1)
 {-# INLINE [3] smap #-}

 szipWith f arr1 (ASmall arr2)
        = ASmall (szipWith f arr1 arr2)
 {-# INLINE [3] szipWith #-}


-- Interleaved ------------------------
instance   Structured r1 a b
        => Structured (I r1) a b where
 type TR (I r1) = I (TR r1)

 smap f (AInterleave arr1)
        = AInterleave (smap f arr1)
 {-# INLINE [3] smap #-}

 szipWith f arr1 (AInterleave arr2)
        = AInterleave (szipWith f arr1 arr2)
 {-# INLINE [3] szipWith #-}


-- Unboxed ----------------------------
instance Unbox a => Structured U a b where
 type TR U = D
 smap           = map
 szipWith       = zipWith


-- Undefined --------------------------
instance Structured X a b where
 type TR X = X
 smap     _   (AUndefined sh) = AUndefined sh
 szipWith _ _ (AUndefined sh) = AUndefined sh

