{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module DArray ( 
    DArray (..)
  , toDArray 
  , toScalar
  , fromDArray
  , forceDArray
  , mkDArray
  , traverseDArray
  , traverse2DArray
  , backpermute
  , backpermuteDft
  , map
  , zip
  , zipWith
  , fold
  , shift
  , reshape
  , rotate
  , transpose
  , tile
  , append
  , select
  , replicate
  , replicateSlice
  , index
  ) where
         
import qualified Data.Array.Parallel.Unlifted as U
import  Data.Array.Parallel.Unlifted ((:*:)(..))
import Data.Array.Parallel.Base (Rebox)

import Data.Array.Parallel.Unlifted.Gabi (mapU,foldU,enumFromToU)

import Array ((:.)(..))
import qualified Array as A
import Prelude hiding (map, zip, zipWith, replicate, sum)

import Debug.Trace

data DArray dim e where 
  DArray:: 
    { darrayShape :: dim
    , darrayFn    :: dim -> e
    } -> DArray dim e

instance (U.Elt e, A.Shape dim, Show e) => Show (DArray dim e) where
  show darr = show $ fromDArray darr

assert a b = b


--  Instances
-- ===========

instance (U.Elt e, A.Shape dim, Num e) => Num (DArray dim e) where
  (+) (DArray sh1 f1) (DArray sh2 f2) 	= DArray sh1 (\sh -> (f1 sh) + f2 sh)
  (-) (DArray sh1 f1) (DArray sh2 f2) 	= DArray sh1 (\sh -> (f1 sh) - f2 sh)
  (*) (DArray sh1 f1) (DArray sh2 f2) 	= DArray sh1 (\sh -> (f1 sh) * f2 sh)
  negate (DArray sh1 f1) 		= DArray sh1 (\sh -> negate (f1 sh))
  abs (DArray sh1 f1) 			= DArray sh1 (\sh -> abs (f1 sh))
  signum (DArray sh1 f1) 		= DArray sh1 (\sh -> signum (f1 sh))
  fromInteger n 			= DArray undefined (\_ -> fromInteger n)

                  
instance (U.Elt e, Eq e, A.Shape sh) => Eq (DArray sh e) where
  (==) arr1@(DArray sh _)  arr2 = 
    toScalar $ fold (&&) True $ (flip reshape) (() :. (A.size sh)) $ zipWith (==) arr1 arr2
  (/=) a1 a2 = not $ (==) a1 a2



--  Constructors
--  ============

-- |Convert a strict array into a delayed array
toDArray:: (U.Elt e, A.Shape dim) => A.Array dim e -> DArray dim e
{-# INLINE toDArray #-}
toDArray (A.Array sh xs) = sh `A.deepSeq` xs `seq`
  DArray sh (\i -> (xs U.!: A.toIndex sh i))

-- |Convert a zer dimensional array into a scalar value
toScalar :: U.Elt e => DArray () e -> e
toScalar (DArray _ fn) = fn ()

-- |Convert delayed array into strict array, force evaluation
fromDArray:: (U.Elt e, A.Shape dim) => DArray dim e -> A.Array dim e
{-# INLINE fromDArray #-}
fromDArray (DArray shape fn)
   = shape `A.deepSeq`
     A.Array { A.arrayData = 
                     -- relative performance of these implementations differs
                     -- depending on ghc optimisations (fusion, inline patch)
                     U.map (fn . i) (U.enumFromTo (0::Int) ((A.size shape) - 1))  
             , A.arrayShape = shape}
     where
       i = A.fromIndex shape

-- |Makes sure the underlying Unboxed.Array is evaluated, and the DArray function
-- is a simple look up
forceDArray:: (U.Elt e, A.Shape dim) => DArray dim e -> DArray dim e
{-# INLINE forceDArray #-}
forceDArray arr@(DArray d _) = 
  A.arrayData arr' `seq` (toDArray arr')
      where  
        arr' = fromDArray arr

mkDArray:: (U.Elt e, A.Shape dim) => dim -> (dim -> e) -> DArray dim e 
{-# INLINE mkDArray #-}
mkDArray d fn = DArray d fn

traverseDArray:: 
  DArray dim e -> (dim -> dim') -> ((dim -> e) -> dim' -> f) -> DArray dim' f
{-# INLINE traverseDArray #-}
traverseDArray (DArray d f) dFn fTrafo = 
  DArray (dFn d) (fTrafo f)


traverse2DArray:: 
  DArray dim e -> DArray dim' f -> 
  (dim -> dim' -> dim'') -> ((dim -> e) -> (dim' -> f) -> (dim'' -> g)) -> DArray dim'' g
{-# INLINE traverse2DArray #-}
traverse2DArray (DArray d f1) (DArray d' f2) dFn fTrafo = 
  DArray (dFn d d') (fTrafo f1 f2)

--  Basic operations
--  =================

-- |Generalised array backpermutation: arguments: delayed array, a target shape
--  and a function mapping each index in the target shape range to an index 
--  of the src array range.
backpermute:: (U.Elt e, A.Shape dim, A.Shape dim') => 
  DArray dim e -> dim' -> (dim' -> dim) -> DArray dim' e
{-# INLINE backpermute #-}
backpermute (DArray shape fn) newSh fn' =
  DArray newSh (fn.fn') 

backpermuteDft::(U.Elt e, A.Shape dim, A.Shape dim') => 
  DArray dim e -> DArray dim' e -> (dim' -> Maybe dim) -> DArray dim' e
{-# INLINE backpermuteDft #-}
backpermuteDft srcArr@(DArray sh fn) dftArr@(DArray dsh dfn)  fn' = 
  DArray dsh fn''
  where
    fn'' i = case (fn' i) of
               Just i' -> fn i'
               Nothing -> dfn i

--  Computations
--  ============

-- | Map function over each element of N-dim DArray
map:: (U.Elt a, U.Elt b) => 
  (a -> b) -> DArray dim a -> DArray dim b
{-# INLINE map #-}
map fn' (DArray shape fn) = 
  DArray shape (fn'.fn)

-- | If the size of two array arguments differ in a dimension, the resulting
--   array's shape is the minimum of the two 
zipWith:: (U.Elt a, U.Elt b, U.Elt c, A.Shape dim) => 
  (a -> b -> c) -> DArray dim a -> DArray dim b-> DArray dim c
{-# INLINE zipWith #-}
zipWith f (DArray shape1 fn1) (DArray shape2 fn2) = 
  DArray (A.intersectDim shape1 shape2) (\i -> f (fn1 i) (fn2 i))



-- | If the size of two array arguments differ in a dimension, the resulting
--   array's shape is the minimum of the two 
zip:: (U.Elt a, U.Elt b, A.Shape dim) => 
  DArray dim a -> DArray dim b-> DArray dim (a :*: b)
{-# INLINE zip #-}
zip (DArray shape1 fn1) (DArray shape2 fn2) = 
  DArray (A.intersectDim shape1 shape2) (\i -> (fn1 i) :*: (fn2 i))
         


-- | folds the innermost dimension. Combine with `transpose to fold any other dimension.
fold :: (U.Elt e, A.Shape dim) => 
 (e -> e-> e) -> e -> DArray (dim :. Int)  e  -> DArray dim e
{-# INLINE fold #-}
fold f n arr@(DArray sh@(sh' :. s) fn) = 
  DArray sh' f'
  where
    f' i = foldU f n (mapU (\s -> fn (i:.s)) (enumFromToU 0 (s-1)))

{-
scan:: (U.Elt e, A.Shape dim) => 
 (e -> e-> e) --> DArray (dim :. Int)  e  -> DArray (dim :. Int)  e 
scan (DArray (sh :. n) f) = 
-}


----  Non-primitive functions 
----  ========================

shift:: (A.Subshape dim dim', U.Elt e) => DArray dim e -> e -> dim' -> DArray dim e
{-# INLINE shift #-}
shift arr@(DArray sh _) e shiftOffset = backpermuteDft arr  (DArray sh (\_ -> e))
  (\d -> if (A.inRange A.zeroDim sh (A.addDim d shiftOffset)) 
           then Just (A.addDim d shiftOffset) 
           else Nothing)

reshape:: (A.Shape dim, A.Shape dim', U.Elt e) => DArray dim e -> dim' -> DArray dim' e
{-# INLINE reshape #-}
reshape arr@(DArray sh fn) newShape = assert (A.size newShape == A.size sh) $
  DArray newShape (fn .  (A.fromIndex sh). (A.toIndex newShape))


rotate:: (A.Subshape dim dim', U.Elt e) => DArray dim e -> e -> dim' -> DArray dim e
{-# INLINE rotate #-}
rotate arr@(DArray sh _) e shiftOffset = backpermute arr  sh
  (\d -> A.addModDim sh d shiftOffset)


transpose:: (A.Shape dim, U.Elt e) => 
  DArray (dim :. Int :. Int) e -> DArray (dim :. Int :. Int) e
{-# INLINE transpose #-}
transpose arr =
  traverseDArray arr
     (\(sh :. m :. n) -> (sh :. n :.m))
     (\f -> \(sh :. i :. j) -> f (sh :. j :. i))


tile::  (A.Subshape dim dim', U.Elt e) => DArray dim e -> dim' -> dim' -> DArray dim e
{-# INLINE tile #-}
tile arr@(DArray sh _) start size = 
--  assert (A.inRange sh (A.addDim start size)) $
     backpermute arr (A.inject sh size)
     (\d -> A.addDim d start)


insertTile:: (A.Subshape dim dim, U.Elt e) => DArray dim e -> DArray dim e  -> dim -> DArray dim e
insertTile arr@(DArray sh f) tile@(DArray shTile fTile) offset = 
  DArray sh f'
  where
    f' d = if (A.inRange offset (A.addDim sh shTile) d)
             then fTile d
             else f d

--  Combining arrays
-- 
append:: (A.Subshape dim dim, U.Elt e) => DArray dim e -> DArray dim e -> dim -> DArray dim e
{-# INLINE append #-}
append arr1@(DArray sh1 fn1) arr2@(DArray sh2 fn2) newSh =
  DArray newSh appFn
  where
    appFn i = if (A.inRange A.zeroDim sh1 i) 
                then fn1 i
                else fn2 (A.modDim i sh1)
  

--  Shape polymorphic ops based on Index
--  ====================================

select:: (U.Elt e, A.Shape dim, A.Shape dim') => 
  DArray dim e -> A.SelectIndex dim dim'  -> DArray dim' e
{-# INLINE select #-}
select arr@(DArray shape _ ) ind = 
  backpermute arr (A.projShape ind shape) (selectFun ind)
  where
    selectFun:: A.SelectIndex dim1 dim2 -> dim2 -> dim1
    selectFun A.IndexNil sh = sh
    selectFun (A.IndexAll rsh) (shs :. s) = (selectFun rsh shs) :. s
    selectFun (A.IndexFixed n rsh) shs     = (selectFun rsh shs) :. n


replicate:: (U.Elt e, A.Shape dim, A.Shape dim', A.InitShape dim, A.RepFun dim) => 
  DArray dim' e -> A.SelectIndex dim dim'  -> DArray dim e
{-# INLINE replicate #-}
replicate arr@(DArray shape _ ) ind = 
  backpermute arr (A.initShape ind shape) (A.repFun ind)


replicateSlice:: (U.Elt e, A.Slice sl, A.Shape (A.FullShape sl), A.Shape (A.SliceShape sl)) => 
  DArray (A.SliceShape sl) e -> sl  -> DArray (A.FullShape sl) e
{-# INLINE replicateSlice #-}
replicateSlice arr@(DArray shape _ ) sl = 
  backpermute arr (A.repShape sl shape) (A.repInd sl)


index::(U.Elt e, A.Shape dim) => DArray dim e -> dim -> e
{-# INLINE index #-}
index arr@(DArray _ fn) i =
    fn i  


